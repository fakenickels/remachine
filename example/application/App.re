open Trex.React;
open Js_of_ocaml;

/*
 let lexbuf =
   Lexing.from_string(
     {|
   type lights = | Red | Green | Blue

   let state = light => switch(light) {
     | Red => Green
     | Green => Blue
     | Blue => Red
   }
 |},
   ); */

let rec find = (predicate, list) => {
  switch (list) {
  | [element, ...tail] =>
    predicate(element) ? element : find(predicate, tail)
  | [] => None
  };
};

let rec findInExpression = expression => {
  Migrate_parsetree.Ast_404.Parsetree.(
    switch (expression) {
    | Pexp_fun(_, _, _, {pexp_desc: ast, _}) => findInExpression(ast)
    | Pexp_match(_expression, cases) =>
      print_endline("Match expression found");
      Some(cases);
    | _ => None
    }
  );
};

module Parsetree = Migrate_parsetree.Ast_404.Parsetree;

let rec findPatternMatching = structure_items => {
  Migrate_parsetree.Ast_404.Parsetree.(
    switch (structure_items) {
    | [{pstr_desc: Pstr_value(_rec_flag, value_bindings), _}, ...rest] =>
      print_endline("Value declaration found");
      value_bindings
      |> List.map(({pvb_expr: {pexp_desc: ast, _}, _}) =>
           findInExpression(ast)
         )
      |> find((!==)(None))
      |> (
        fun
        | Some(value) => Some(value)
        | None => findPatternMatching(rest)
      );

    | [_, ...rest] =>
      print_endline("Keep looking");
      findPatternMatching(rest);
    | _ => None
    }
  );
};

let casesToGraphviz = (cases: list(Parsetree.case)) => {
  open Parsetree;
  open Migrate_parsetree.Ast_404.Asttypes;

  let rec casesToGraphviz' = (cases: list(Parsetree.case)) => {
    switch (cases) {
    | [
        {
          pc_lhs: {ppat_desc: Ppat_construct({txt: Lident(name), _}, _), _},
          pc_rhs: {
            pexp_desc: Pexp_construct({txt: Lident(nameRight), _}, _),
            _,
          },
          _,
        },
        ...rest,
      ] =>
      let dots = name ++ " -> " ++ nameRight;
      dots ++ "\n" ++ casesToGraphviz'(rest);
    | [
        {
          pc_lhs: {
            ppat_desc:
              Ppat_tuple([
                {
                  ppat_desc: Ppat_construct({txt: Lident(stateName), _}, _),
                  _,
                },
                {
                  ppat_desc: Ppat_construct({txt: Lident(actionName), _}, _),
                  _,
                },
              ]),
            _,
          },
          pc_rhs: {
            pexp_desc: Pexp_construct({txt: Lident(nameRight), _}, _),
            _,
          },
          _,
        },
        ...rest,
      ] =>
      let dots =
        "\t"
        ++ stateName
        ++ " -> "
        ++ nameRight
        ++ {|[ label="|}
        ++ actionName
        ++ {|" ]|};
      dots ++ "\n" ++ casesToGraphviz'(rest);
    | _ => ""
    };
  };

  "digraph {\n" ++ casesToGraphviz'(cases) ++ "}";
};

let%component make = () => {
  let%hook (code, setCode) =
    Trex.Hooks.state(
      {|
    type elapsed = float;

    type taskStatus =
      | NotStarted
      | Running(elapsed)
      | Paused(elapsed)
      | Done(elapsed);

    type input =
      | Start
      | Pause
      | Resume
      | Finish
      | Tick(elapsed);

    let transition = (input, state) =>
      switch (state, input) {
      | (NotStarted, Start) => Running(0.0)
      | (Running(elapsed), Pause) => Paused(elapsed)
      | (Running(elapsed), Finish) => Done(elapsed)
      | (Paused(elapsed), Resume) => Running(elapsed)
      | (Paused(elapsed), Finish) => Done(elapsed)
      | (Running(elapsed), Tick(tick)) => Running(elapsed +. tick)
      | _ => state
      };
  |},
    );
  let%hook (dots, setDots) = Trex.Hooks.state("");

  let createChart = () => {
    let lexbuf = Lexing.from_string(code);

    let ast = Reason_toolchain.RE.use_file(lexbuf);

    let cases =
      ast
      |> List.map(ast => {
           switch (ast) {
           | Parsetree.Ptop_def(structure_items) =>
             findPatternMatching(structure_items)
           | _ =>
             print_endline("No compatible toplevel structures");
             None;
           }
         })
      |> find((!==)(None));

    let _ =
      switch (cases) {
      | Some(cases) =>
        let dots = casesToGraphviz(cases);
        print_string(dots);
        setDots(_ => dots);

        Js.Unsafe.global##.d3##select(Js.string("#my-chart"))##graphviz()##renderDot(
          Js.string(dots),
        );
      | None => print_endline("No cases found")
      };
    ();
  };

  <div className="grid grid-cols-2 divide-y-1 h-screen">
    <div className="p-5">
      <div className="block bg-gray-100">
      <input
        className="my-input w-full h-1/2"
        value=code
        onChange={event => {
          let target = event##.target;

          switch (Js.Opt.to_option(target)) {
          | Some(target) =>
            let node: Js.t(Dom_html.inputElement) = Obj.magic(target);

            setCode(_ => node##.value |> Js.to_string);
          | None => ()
          };
        }}
      />
      </div>
      <div className="block my-5">
        <span
          onClick={_ => createChart()}
          className="bg-gray-100 rounded-full px-3 py-1 text-sm font-semibold text-gray-700 mr-2 cursor-pointer select-none"
          text="Generate"
        />
      </div>
      <div className="bg-gray-100 pl-10"> <span className="chart whitespace-pre" text=dots /> </div>
    </div>
    <div className="flex items-center justify-center">
      <div id="my-chart"> <span text="" /> </div>
    </div>
  </div>;
};