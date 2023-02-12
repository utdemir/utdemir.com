import lume from "lume/mod.ts";
import date from "lume/plugins/date.ts";
import terser from "lume/plugins/terser.ts";
import postcss from "lume/plugins/postcss.ts";

import prism from "lume/plugins/prism.ts";
import "npm:prismjs@1.29.0/components/prism-haskell.js";
import "npm:prismjs@1.29.0/components/prism-markdown.js";
import "npm:prismjs@1.29.0/components/prism-typescript.js";

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
site.use(
  prism()
);
site.use(date());
site.use(terser({
  options: {
    "compress": true,
    "mangle": true,
  }
}));
site.use(postcss());

site.copy("images")
site.copy("assets")

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
            date: date.toISOString().slice(0, "YYYY-MM-DD".length),
            title: page.data.title as string,
            target: page.data.url as string,
            target_absolute: BASE_URL + page.data.url as string,
            type: "post",
            rfc822_date: date.toUTCString(),
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
        rfc822_date: new Date(it.date).toUTCString(),
    }
    activities.push(ret)
  }
  return activities
}
site.helper("get_activities", get_activities, { "type": "filter" });

// Finalize
export default site;
