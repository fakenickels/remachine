module Dom = Js_of_ocaml.Dom;
module Dom_html = Js_of_ocaml.Dom_html;
module Js = Js_of_ocaml.Js;

module RemoteAction = Brisk_reconciler.RemoteAction;

// type hostElement = Js.t(Dom_html.element);
type node = Js.t(Dom_html.element);

let onStale: RemoteAction.t(unit) = RemoteAction.create();

let insertNode = (~parent: node, ~child: node, ~position as _) => {
  Dom.appendChild(parent, child);
  parent;
};

let deleteNode = (~parent: node, ~child: node, ~position as _) => {
  Dom.removeChild(parent, child);
  parent;
};

let moveNode = (~parent, ~child as _, ~from as _, ~to_ as _) => {
  parent;
};

Brisk_reconciler.addStaleTreeHandler(() =>
  RemoteAction.send(~action=(), onStale)
);

open Brisk_reconciler;

let document = Dom_html.window##.document;

let%nativeComponent div = (~children, ~id: string="", ~className: string="", (), hooks) => (
  {
    make: () => {
      let node = Dom_html.createDiv(document);
      node##.className := Js.string(className);
      node##.id := Js.string(id);
      node;
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

let%nativeComponent span = (~className="", ~text="", ~onClick=?, (), hooks) => (
  {
    make: () => {
      let node = Dom_html.createSpan(document);
      node##.className := Js.string(className);
      node##.innerHTML := Js.string(text);
      switch (onClick) {
      | None => ()
      | Some(onClick) =>
        node##.onclick :=
          Dom_html.handler(_e => {
            onClick();
            Js.bool(false);
          })
      };
      node |> Dom_html.element;
    },
    configureInstance: (~isFirstRender as _, n) => {
      /* TODO: Proper way to downcast? */
      let node: Js.t(Dom_html.element) = Obj.magic(n);
      node##.className := Js.string(className);
      node##.innerHTML := Js.string(text);
      switch (onClick) {
      | None => ()
      | Some(onClick) =>
        node##.onclick :=
          Dom_html.handler(_e => {
            onClick();
            Js.bool(false);
          })
      };
      node |> Dom_html.element;
    },
    children: empty,
    insertNode,
    deleteNode,
    moveNode,
  },
  hooks,
);

let%nativeComponent img = (~className, ~src, (), hooks) => (
  {
    make: () => {
      let node = Dom_html.createImg(document);
      node##.className := Js.string(className);
      node##.src := Js.string(src);
      node |> Dom_html.element;
    },
    configureInstance: (~isFirstRender as _, node) => {
      /* TODO: Proper way to downcast? */
      let node: Js.t(Dom_html.imageElement) = Obj.magic(node);
      node##.src := Js.string(src);
      node |> Dom_html.element;
    },
    children: Brisk_reconciler.empty,
    insertNode,
    deleteNode,
    moveNode,
  },
  hooks,
);

let%nativeComponent input = (~className, ~value, ~onChange, (), hooks) => (
  {
    make: () => {
      let node = Dom_html.createTextarea(document);
      node##.className := Js.string(className);
      node##.value := Js.string(value);
      node##.onchange := Dom_html.handler(e => {
        onChange(e);
        Js.bool(false);
      });
      node |> Dom_html.element;
    },
    configureInstance: (~isFirstRender as _, node) => {
      /* TODO: Proper way to downcast? */
      let node: Js.t(Dom_html.inputElement) = Obj.magic(node);
      node##.value := Js.string(value);
      node##.onchange := Dom_html.handler(e => {
        onChange(e);
        Js.bool(false);
      });
      node |> Dom_html.element;
    },
    children: Brisk_reconciler.empty,
    insertNode,
    deleteNode,
    moveNode,
  },
  hooks,
);

let render = application => {
  let node = Dom_html.getElementById_exn("root");
  // Note: clear node from SSR content
  node##.innerHTML := Js.string("");

  let rendered =
    ref(
      RenderedElement.render(
        {node, insertNode, deleteNode, moveNode},
        application,
      ),
    );

  RenderedElement.executeHostViewUpdates(rendered^) |> ignore;

  let _unsubscribe =
    RemoteAction.subscribe(
      ~handler=
        () => {
          let nextElement = RenderedElement.flushPendingUpdates(rendered^);
          RenderedElement.executeHostViewUpdates(nextElement) |> ignore;
          rendered := nextElement;
        },
      onStale,
    );

  ();
};

let renderToString = _application => "This is no-op on jsoo";
