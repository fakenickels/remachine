module RemoteAction = Brisk_reconciler.RemoteAction;

type node =
  | Div(layoutNode)
  | Span(layoutNode)
  | Image({
      className: string,
      src: string,
    })
  | Text({text: string})
and layoutNode = {
  className: option(string),
  children: list(node),
};

let insertNode = (~parent: node, ~child: node, ~position as _) => {
  switch (parent) {
  | Span(node) => Span({...node, children: node.children @ [child]})
  | Div(node) => Div({...node, children: node.children @ [child]})
  | parent => parent
  };
};
let deleteNode = (~parent: node, ~child: node, ~position as _) => {
  switch (parent) {
  | Div(node) =>
    Div({...node, children: node.children |> List.filter(c => c === child)})
  | Span(node) =>
    Div({...node, children: node.children |> List.filter(c => c === child)})
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

/*
   Step 2: Define some native components (aka primitives)
 */
let%nativeComponent div = (~children, ~className=?, (), hooks) => (
  {
    make: () => {
      Div({className, children: []});
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
      Span({className, children: [Text({text: text})]});
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

let render = _application => {
  Logs.warn(m => m("This is no-op on native"));
};

let render_to_string = application => {
  let rendered =
    ref(
      Brisk_reconciler.RenderedElement.render(
        {
          node: Div({className: None, children: []}),
          insertNode,
          deleteNode,
          moveNode,
        },
        application,
      ),
    );

  let hostView =
    Brisk_reconciler.RenderedElement.executeHostViewUpdates(rendered^);

  let or_else = default =>
    fun
    | None => default
    | Some(a) => a;

  let rec toHtml =
    fun
    | Div(node) =>
      "<div"
      ++ (
        node.className
        |> Option.map(v => Printf.sprintf(" class=\"%s\"", v))
        |> or_else("")
      )
      ++ ">"
      ++ (node.children |> List.map(toHtml) |> List.fold_left((++), ""))
      ++ "</div>"
    | Image(node) =>
      "<img"
      ++ (
        " class=\"" ++ node.className ++ "\"" ++ " src=\"" ++ node.src ++ "\""
      )
      ++ ">"
    | Span(node) =>
      "<span"
      ++ (
        node.className
        |> Option.map(v => Printf.sprintf(" class=\"%s\"", v))
        |> or_else("")
      )
      ++ ">"
      ++ (node.children |> List.map(toHtml) |> List.fold_left((++), ""))
      ++ "</span>"
    | Text(node) => node.text;

  {|
    <!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/tailwindcss/1.2.0/base.min.css"/>
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/tailwindcss/1.2.0/utilities.min.css"/>
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/tailwindcss/1.2.0/components.min.css"/>
  <title>Trex</title>
</head><body id="root">|}
  ++ toHtml(hostView)
  ++ {|
  <script type="application/javascript" src="static/_esy/default/build/default/executable/client/Client.bc.js"></script>
  </body></html>|};
};
