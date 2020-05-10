module RemoteAction = Brisk_reconciler.RemoteAction;

type node =
  | Empty(list(node))
  | Div(layoutNode)
  | Span(layoutNode)
  | Image({
      className: string,
      src: string,
    })
  | Text({text: string})
  | Input({className: string})
and layoutNode = {
  className: option(string),
  id: option(string),
  children: list(node),
};

let insertNode = (~parent: node, ~child: node, ~position as _) => {
  switch (parent) {
  | Empty(children) => Empty(children @ [child])
  | Span(node) => Span({...node, children: node.children @ [child]})
  | Div(node) => Div({...node, children: node.children @ [child]})
  | parent => parent
  };
};

let deleteNode = (~parent: node, ~child: node, ~position as _) => {
  switch (parent) {
  | Empty(children) => Empty(children |> List.filter(c => c === child))
  | Div(node) =>
    Div({...node, children: node.children |> List.filter(c => c === child)})
  | Span(node) =>
    Span({...node, children: node.children |> List.filter(c => c === child)})
  | parent => parent
  };
};

let moveNode = (~parent, ~child as _, ~from as _, ~to_ as _) => {
  parent;
};

let onStale = RemoteAction.create();
Brisk_reconciler.addStaleTreeHandler(() =>
  RemoteAction.send(~action=(), onStale)
);

let%nativeComponent div = (~children, ~id=?, ~className=?, (), hooks) => (
  {
    make: () => {
      Div({className, id, children: []});
    },
    configureInstance: (~isFirstRender as _, node) => {
      node;
    },
    children,
    insertNode,
    deleteNode,
    moveNode,
  },
  hooks,
);

let%nativeComponent span =
                    (
                      ~className=?,
                      ~text="",
                      ~onClick as _: option(unit => unit)=?,
                      (),
                      hooks,
                    ) => (
  {
    make: () => {
      Span({className, id: None, children: [Text({text: text})]});
    },
    configureInstance: (~isFirstRender as _, node) => {
      node;
    },
    children: Brisk_reconciler.empty,
    insertNode,
    deleteNode,
    moveNode,
  },
  hooks,
);

let%nativeComponent img = (~className, ~src, (), hooks) => (
  {
    make: () => {
      Image({className, src});
    },
    configureInstance: (~isFirstRender as _, node) => {
      node;
    },
    children: Brisk_reconciler.empty,
    insertNode,
    deleteNode,
    moveNode,
  },
  hooks,
);

let%nativeComponent input = (~className, ~value as _, ~onChange as _, (), hooks) => (
  {
    make: () => {
      Input({className: className});
    },
    configureInstance: (~isFirstRender as _, node) => {
      node;
    },
    children: Brisk_reconciler.empty,
    insertNode,
    deleteNode,
    moveNode,
  },
  hooks,
);

let render = _application => {
  Logs.warn(m => m("This is no-op on native"));
};

let renderToString = application => {
  let rendered =
    ref(
      Brisk_reconciler.RenderedElement.render(
        {node: Empty([]), insertNode, deleteNode, moveNode},
        application,
      ),
    );

  let hostView =
    Brisk_reconciler.RenderedElement.executeHostViewUpdates(rendered^);

  let or_else = default =>
    fun
    | None => default
    | Some(a) => a;

  let rec toHtml = (node: node): list(string) =>
    switch (node) {
    | Empty(children) => children |> List.map(toHtml) |> List.flatten
    | Div(node) =>
      List.concat([
        [
          "<div",
          node.className
          |> Option.map(v => Printf.sprintf(" class=\"%s\"", v))
          |> or_else(""),
          ">",
        ],
        node.children |> List.map(toHtml) |> List.flatten,
        ["</div>"],
      ])
    | Image(node) => [
        "<img",
        " class=\"" ++ node.className ++ "\"",
        " src=\"" ++ node.src ++ "\"",
        ">",
      ]
    | Input(node) => [
        "<textarea",
        " class=\"" ++ node.className ++ "\"",
        "></textarea>",
      ]
    | Span(node) =>
      List.concat([
        [
          "<span",
          node.className
          |> Option.map(v => Printf.sprintf(" class=\"%s\"", v))
          |> or_else(""),
          ">",
        ],
        node.children |> List.map(toHtml) |> List.flatten,
        ["</span>"],
      ])
    | Text(node) => [node.text]
    };

  List.concat([
    [
      {|<!DOCTYPE html>|},
      {|<html lang="en">|},
      {|<head>|},
      {|<meta charset="UTF-8">|},
      {|<meta name="viewport" content="width=device-width, initial-scale=1.0">|},
      {|<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/tailwindcss/1.2.0/base.min.css"/>|},
      {|<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/tailwindcss/1.2.0/utilities.min.css"/>|},
      {|<link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/tailwindcss/1.2.0/components.min.css"/>|},
      {|<title>Trex</title></head><body id="root">|},
    ],
    hostView |> toHtml,
    [
      {|<script type="application/javascript" src="static/_esy/default/build/default/example/client/Client.bc.js"></script>|},
      {|</body></html>|},
    ],
  ])
  |> List.fold_left((++), "");
};
