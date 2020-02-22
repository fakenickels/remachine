open Lib.Util;

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

  switch (request.meth, path_parts) {
  | (`GET, _) =>
    Morph.Response.html(
      render_to_string(() =>
        <div className="max-w-sm rounded overflow-hidden shadow-lg">
          <div className="px-6 py-4">
            <div className="font-bold text-xl mb-2">
              {string("The Coldest Sunset")}
            </div>
            <span className="text-gray-700 text-base">
              {string(
                 "Lorem ipsum dolor sit amet, consectetur adipisicing elit. Voluptatibus quia, nulla! Maiores et perferendis eaque, exercitationem praesentium nihil.",
               )}
            </span>
          </div>
          <div className="px-6 py-4">
            <span
              className="inline-block bg-gray-200 rounded-full px-3 py-1 text-sm font-semibold text-gray-700 mr-2">
              {string("#photography")}
            </span>
            <span
              className="inline-block bg-gray-200 rounded-full px-3 py-1 text-sm font-semibold text-gray-700 mr-2">
              {string("#travel")}
            </span>
            <span
              className="inline-block bg-gray-200 rounded-full px-3 py-1 text-sm font-semibold text-gray-700">
              {string("#winter")}
            </span>
          </div>
        </div>
      ),
      Morph.Response.empty,
    )
  | (_, _) => Morph.Response.not_found(Morph.Response.empty)
  };
};

let http_server = Morph_server_http.make();

let () =
  handler
  |> Morph.start(~servers=[http_server], ~middlewares=[])
  |> Lwt_main.run;
