import lume from "lume/mod.ts";
import date from "lume/plugins/date.ts";
import terser from "lume/plugins/terser.ts";
import inline from "lume/plugins/inline.ts";
import minifyHTML from "lume/plugins/minify_html.ts";
import sitemap from "lume/plugins/sitemap.ts";

import dayjs from "npm:dayjs";

import prism from "lume/plugins/prism.ts";
import "prism/components/prism-haskell.js";
import "prism/components/prism-markdown.js";
import "prism/components/prism-typescript.js";

const BASE_URL = "https://utdemir.com"

const site = lume(
  {
    prettyUrls: false
  },
  {
    nunjucks: {
      options: {
        throwOnUndefined: true,
      },
    },
  }
);

// Load plugins
site.use(prism());
site.use(date());
site.use(terser({
  options: {
    "compress": true,
    "mangle": true,
  }
}));
site.use(minifyHTML());
site.use(inline());
site.use(sitemap());

// Assets

site.copy("images")
site.copy("assets")

site.copy("favicon.ico")

// Per-post extra data

site.copy("assets/zsh-sticky-prefix/zsh-sticky-prefix.gif")

// Populate activities
type ExternalActivity = {
  date: string;
  title: string;
  target: string;
  type: string;
};

type Activity = {
  date: string;
  rfc822_date: string;
  title: string;
  target: string;
  target_absolute: string;
  type: string;
  external: boolean;
};

const external_activities: ExternalActivity[] = JSON.parse(
    await Deno.readTextFile("./activity.json")
)

function get_activities() {
  const activities: Activity[] = [];
  for (const page of site.pages) {
    if(page.src.path.startsWith("/posts") && page.data.published) {
        const date = page.data.date as Date;
        const ret = {
            date: dayjs(date).format("MMM 'YY"),
            title: page.data.title as string,
            target: page.data.url as string,
            target_absolute: BASE_URL + page.data.url as string,
            type: "post",
            rfc822_date: date.toISOString(),
            external: false,
        };
        activities.push(ret);
    }
  }
  for (const it of external_activities) {
    const ret = {
        ...it,
        target_absolute: it.target,
        external: true,
        date: dayjs(it.date).format("MMM 'YY"),
        rfc822_date: new Date(it.date).toISOString(),
    }
    activities.push(ret)
  }
  return activities
}
site.helper("get_activities", get_activities, { "type": "filter" });

// Finalize
export default site;
