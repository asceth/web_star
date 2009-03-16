%% This is the application resource file (.app fle) for the web_star,
%% application.
{application, web_star,
  [{description, "Web Star Application"},
   {vsn, "0.1.0"},
   {modules, [web_star_app,
              web_star_sup,
              web_star,
              web_star_cycle,
              web_application]},
   {registered, [web_star_sup]},
   {applications, [kernel, stdlib]},
   {mod, {web_star_app, []}},
   {start_phases, []}]}.
