open Trex.React;

Fmt_tty.setup_std_outputs();
Logs.set_level(Some(Logs.Info));
Logs.set_reporter(Logs_fmt.reporter());

let handler = (request: Morph.Request.t) => {
  /* TODO: move to Morph as Morph.Uri.split ??? */
  let path_parts =
    request.target
    |> Uri.of_string
    |> Uri.path
    |> String.split_on_char('/')
    |> List.filter(s => s != "");

  Logs.info(m => m("request.target: %s", request.target));

  switch (request.meth, path_parts) {
  | (`GET, []) =>
    Morph.Response.html(renderToString(<App />), Morph.Response.empty)

  | (`GET, ["static", ...path]) =>
    let filepath =
      path |> List.fold_left((acc, part) => acc ++ "/" ++ part, Sys.getcwd());

    Morph.Response.static(filepath, Morph.Response.empty);

  | (_, _) => Morph.Response.not_found(Morph.Response.empty)
  };
};

let http_server = Morph_server_http.make();

let () =
  handler
  |> Morph.start(~servers=[http_server], ~middlewares=[])
  |> Lwt_main.run;
