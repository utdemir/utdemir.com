const e = React.createElement

const ASSET_ROOT = "/assets/choosing-cloud-regions";

const LATENCY_MAPS = [[],[{"dcs":["eu-west-2"],"cost":33.5232,"fname":"eu-west-2","median_latency":153},{"dcs":["ap-northeast-3"],"cost":36.403200000000005,"fname":"ap-northeast-3","median_latency":157},{"dcs":["eu-central-1"],"cost":33.5232,"fname":"eu-central-1","median_latency":160},{"dcs":["eu-west-3"],"cost":34.992,"fname":"eu-west-3","median_latency":163},{"dcs":["eu-west-1"],"cost":29.1456,"fname":"eu-west-1","median_latency":168},{"dcs":["eu-south-1"],"cost":34.992,"fname":"eu-south-1","median_latency":175},{"dcs":["me-south-1"],"cost":37.872,"fname":"me-south-1","median_latency":176},{"dcs":["eu-north-1"],"cost":32.04,"fname":"eu-north-1","median_latency":184},{"dcs":["us-west-2"],"cost":29.1456,"fname":"us-west-2","median_latency":189},{"dcs":["ap-south-1"],"cost":30.643200000000004,"fname":"ap-south-1","median_latency":197}],[{"dcs":["eu-west-2","ap-southeast-1"],"cost":34.9632,"fname":"eu-west-2_ap-southeast-1","median_latency":86},{"dcs":["eu-west-3","ap-southeast-1"],"cost":35.6976,"fname":"eu-west-3_ap-southeast-1","median_latency":89},{"dcs":["eu-west-2","ap-southeast-3"],"cost":34.9632,"fname":"eu-west-2_ap-southeast-3","median_latency":91},{"dcs":["ap-northeast-3","eu-west-2"],"cost":34.9632,"fname":"ap-northeast-3_eu-west-2","median_latency":92},{"dcs":["eu-central-1","ap-southeast-1"],"cost":34.9632,"fname":"eu-central-1_ap-southeast-1","median_latency":92},{"dcs":["ap-northeast-3","eu-west-3"],"cost":35.6976,"fname":"ap-northeast-3_eu-west-3","median_latency":93},{"dcs":["eu-west-3","ap-southeast-3"],"cost":35.6976,"fname":"eu-west-3_ap-southeast-3","median_latency":93},{"dcs":["ap-northeast-3","eu-central-1"],"cost":34.9632,"fname":"ap-northeast-3_eu-central-1","median_latency":96},{"dcs":["eu-central-1","ap-southeast-3"],"cost":34.9632,"fname":"eu-central-1_ap-southeast-3","median_latency":96},{"dcs":["eu-west-1","ap-southeast-1"],"cost":32.7744,"fname":"eu-west-1_ap-southeast-1","median_latency":96}],[{"dcs":["ap-northeast-3","eu-central-1","ap-southeast-1"],"cost":35.4432,"fname":"ap-northeast-3_eu-central-1_ap-southeast-1","median_latency":71},{"dcs":["us-east-2","eu-central-1","ap-southeast-1"],"cost":33.024,"fname":"us-east-2_eu-central-1_ap-southeast-1","median_latency":71},{"dcs":["us-east-1","eu-central-1","ap-southeast-1"],"cost":33.024,"fname":"us-east-1_eu-central-1_ap-southeast-1","median_latency":71},{"dcs":["eu-central-1","ap-east-1","ap-southeast-1"],"cost":36.652800000000006,"fname":"eu-central-1_ap-east-1_ap-southeast-1","median_latency":71},{"dcs":["ap-northeast-3","eu-west-2","ap-southeast-1"],"cost":35.4432,"fname":"ap-northeast-3_eu-west-2_ap-southeast-1","median_latency":72},{"dcs":["ap-northeast-3","eu-west-3","ap-southeast-1"],"cost":35.93280000000001,"fname":"ap-northeast-3_eu-west-3_ap-southeast-1","median_latency":72},{"dcs":["ca-central-1","eu-central-1","ap-southeast-1"],"cost":34.0032,"fname":"ca-central-1_eu-central-1_ap-southeast-1","median_latency":72},{"dcs":["eu-west-2","us-east-1","ap-southeast-1"],"cost":33.024,"fname":"eu-west-2_us-east-1_ap-southeast-1","median_latency":72},{"dcs":["eu-west-2","ap-east-1","ap-southeast-1"],"cost":36.652800000000006,"fname":"eu-west-2_ap-east-1_ap-southeast-1","median_latency":72},{"dcs":["us-west-1","eu-central-1","ap-southeast-1"],"cost":34.483200000000004,"fname":"us-west-1_eu-central-1_ap-southeast-1","median_latency":72}],[{"dcs":["ca-central-1","eu-central-1","ap-east-1","ap-southeast-1"],"cost":35.510400000000004,"fname":"ca-central-1_eu-central-1_ap-east-1_ap-southeast-1","median_latency":63},{"dcs":["us-east-2","eu-central-1","ap-east-1","ap-southeast-1"],"cost":34.776,"fname":"us-east-2_eu-central-1_ap-east-1_ap-southeast-1","median_latency":63},{"dcs":["us-east-1","eu-central-1","ap-east-1","ap-southeast-1"],"cost":34.776,"fname":"us-east-1_eu-central-1_ap-east-1_ap-southeast-1","median_latency":63},{"dcs":["ap-northeast-3","ca-central-1","eu-central-1","ap-southeast-1"],"cost":34.6032,"fname":"ap-northeast-3_ca-central-1_eu-central-1_ap-southeast-1","median_latency":64},{"dcs":["ap-northeast-3","eu-west-2","us-east-2","ap-southeast-1"],"cost":33.8688,"fname":"ap-northeast-3_eu-west-2_us-east-2_ap-southeast-1","median_latency":64},{"dcs":["ap-northeast-3","us-west-1","eu-central-1","ap-southeast-1"],"cost":34.9632,"fname":"ap-northeast-3_us-west-1_eu-central-1_ap-southeast-1","median_latency":64},{"dcs":["ap-northeast-3","us-east-2","eu-central-1","ap-southeast-1"],"cost":33.8688,"fname":"ap-northeast-3_us-east-2_eu-central-1_ap-southeast-1","median_latency":64},{"dcs":["ap-northeast-3","us-east-1","eu-central-1","ap-southeast-1"],"cost":33.8688,"fname":"ap-northeast-3_us-east-1_eu-central-1_ap-southeast-1","median_latency":64},{"dcs":["ca-central-1","eu-west-2","ap-east-1","ap-southeast-1"],"cost":35.510400000000004,"fname":"ca-central-1_eu-west-2_ap-east-1_ap-southeast-1","median_latency":64},{"dcs":["eu-west-2","us-east-2","ap-east-1","ap-southeast-1"],"cost":34.776,"fname":"eu-west-2_us-east-2_ap-east-1_ap-southeast-1","median_latency":64}],[{"dcs":["us-east-2","eu-central-1","ap-east-1","ap-southeast-1","ap-south-1"],"cost":33.94944,"fname":"us-east-2_eu-central-1_ap-east-1_ap-southeast-1_ap-south-1","median_latency":55},{"dcs":["us-east-1","eu-central-1","ap-east-1","ap-southeast-1","ap-south-1"],"cost":33.94944,"fname":"us-east-1_eu-central-1_ap-east-1_ap-southeast-1_ap-south-1","median_latency":55},{"dcs":["ca-central-1","eu-central-1","ap-east-1","ap-southeast-1","ap-south-1"],"cost":34.53696000000001,"fname":"ca-central-1_eu-central-1_ap-east-1_ap-southeast-1_ap-south-1","median_latency":56},{"dcs":["ap-northeast-3","ca-central-1","eu-central-1","ap-east-1","ap-south-1"],"cost":34.53696000000001,"fname":"ap-northeast-3_ca-central-1_eu-central-1_ap-east-1_ap-south-1","median_latency":57},{"dcs":["ap-northeast-3","ca-central-1","eu-central-1","ap-southeast-1","ap-south-1"],"cost":33.8112,"fname":"ap-northeast-3_ca-central-1_eu-central-1_ap-southeast-1_ap-south-1","median_latency":57},{"dcs":["ap-northeast-3","us-east-2","eu-central-1","ap-east-1","ap-south-1"],"cost":33.94944,"fname":"ap-northeast-3_us-east-2_eu-central-1_ap-east-1_ap-south-1","median_latency":57},{"dcs":["ap-northeast-3","us-east-2","eu-central-1","ap-southeast-1","ap-south-1"],"cost":33.22368,"fname":"ap-northeast-3_us-east-2_eu-central-1_ap-southeast-1_ap-south-1","median_latency":57},{"dcs":["ap-northeast-3","us-east-1","eu-central-1","ap-east-1","ap-south-1"],"cost":33.94944,"fname":"ap-northeast-3_us-east-1_eu-central-1_ap-east-1_ap-south-1","median_latency":57},{"dcs":["ap-northeast-3","us-east-1","eu-central-1","ap-southeast-1","ap-south-1"],"cost":33.22368,"fname":"ap-northeast-3_us-east-1_eu-central-1_ap-southeast-1_ap-south-1","median_latency":57},{"dcs":["ca-central-1","eu-west-3","ap-east-1","ap-southeast-1","ap-south-1"],"cost":34.83072,"fname":"ca-central-1_eu-west-3_ap-east-1_ap-southeast-1_ap-south-1","median_latency":57}],[{"dcs":["us-east-2","eu-central-1","ap-northeast-1","ap-east-1","ap-southeast-1","ap-south-1"],"cost":34.3584,"fname":"us-east-2_eu-central-1_ap-northeast-1_ap-east-1_ap-southeast-1_ap-south-1","median_latency":49},{"dcs":["us-east-1","eu-central-1","ap-northeast-1","ap-east-1","ap-southeast-1","ap-south-1"],"cost":34.3584,"fname":"us-east-1_eu-central-1_ap-northeast-1_ap-east-1_ap-southeast-1_ap-south-1","median_latency":49},{"dcs":["ca-central-1","eu-central-1","ap-northeast-1","ap-east-1","ap-southeast-1","ap-south-1"],"cost":34.848000000000006,"fname":"ca-central-1_eu-central-1_ap-northeast-1_ap-east-1_ap-southeast-1_ap-south-1","median_latency":50},{"dcs":["us-east-1","eu-central-1","sa-east-1","ap-east-1","ap-southeast-1","ap-south-1"],"cost":36.6432,"fname":"us-east-1_eu-central-1_sa-east-1_ap-east-1_ap-southeast-1_ap-south-1","median_latency":50},{"dcs":["us-east-1","eu-west-3","ap-northeast-1","ap-east-1","ap-southeast-1","ap-south-1"],"cost":34.6032,"fname":"us-east-1_eu-west-3_ap-northeast-1_ap-east-1_ap-southeast-1_ap-south-1","median_latency":50},{"dcs":["ap-northeast-3","ca-central-1","eu-central-1","ap-east-1","ap-southeast-1","ap-south-1"],"cost":34.848000000000006,"fname":"ap-northeast-3_ca-central-1_eu-central-1_ap-east-1_ap-southeast-1_ap-south-1","median_latency":51},{"dcs":["ap-northeast-3","us-east-2","eu-central-1","ap-east-1","ap-southeast-1","ap-south-1"],"cost":34.3584,"fname":"ap-northeast-3_us-east-2_eu-central-1_ap-east-1_ap-southeast-1_ap-south-1","median_latency":51},{"dcs":["ap-northeast-3","us-east-1","eu-central-1","ap-east-1","ap-southeast-1","ap-south-1"],"cost":34.3584,"fname":"ap-northeast-3_us-east-1_eu-central-1_ap-east-1_ap-southeast-1_ap-south-1","median_latency":51},{"dcs":["ca-central-1","eu-central-1","sa-east-1","ap-east-1","ap-southeast-1","ap-south-1"],"cost":37.1328,"fname":"ca-central-1_eu-central-1_sa-east-1_ap-east-1_ap-southeast-1_ap-south-1","median_latency":51},{"dcs":["ca-central-1","eu-west-3","ap-northeast-1","ap-east-1","ap-southeast-1","ap-south-1"],"cost":35.092800000000004,"fname":"ca-central-1_eu-west-3_ap-northeast-1_ap-east-1_ap-southeast-1_ap-south-1","median_latency":51}]]

// WORLD POPULATION DISPLAY

const WORLD_POPULATION_DISPLAY_IMAGES = [
    { fname: "world_population", text: "Population count (GPWv4)" },
    { fname: "country_internet_users", text: "Individual using the Internet per Country (World Bank)" },
    { fname: "world_internet_users", text: "Internet users" },
];

function WorldPopulationDisplay() {
    const [selectedImage, setSelectedImage] = React.useState(WORLD_POPULATION_DISPLAY_IMAGES[2].fname);

    function handle_change() {
        setSelectedImage(event.target.value);
    }

    const obj = WORLD_POPULATION_DISPLAY_IMAGES.filter((i) => i.fname == selectedImage)[0];
    
    return e("div", {},
        e("form", { className: "pure-form", onChange: handle_change }, 
            e("div", { className: "pure-group" },
                e("select", { defaultValue: selectedImage, style: { width: "100%" }}, 
                    WORLD_POPULATION_DISPLAY_IMAGES.map((im) => 
                        e("option", { key: im.fname, value: im.fname }, im.text ),
                    )
                )
            ),
            e("div", { className: "pure-group" }, 
                e("img", { src: ASSET_ROOT + "/" + obj.fname + ".webp" }, null),
            )
        ),
    );
}

ReactDOM.createRoot(document.getElementById("world_population_display")).render(e(WorldPopulationDisplay, null, null));

// DC RANKINGS

function DCRanking(props) {
    const curr = LATENCY_MAPS[props.size];

    const [selectedIx, setSelectedIx] = React.useState(0);

    function changed(arg) {
        const value = arg.target.value;
        setSelectedIx(value);
    }

    const selectedSrc = ASSET_ROOT + "/lm/" + curr[selectedIx].fname + ".webp"

    return e("div", { className: "pure-g" },
        e("div", { className: "pure-u-1" + (props.size == 1 ? " pure-u-lg-1-3" : "") }, 
            e("table", 
                { 
                    className: "pure-table pure-table-horizontal",
                    style: {
                        "width": "100%",
                        "textAlign": "center"
                    }
                },
                e("thead", null, 
                    e("tr", null,
                        e("th", null, "Rank"),
                        e("th", null, props.size == 1 ? "Region" : "Regions"),
                        e("th", null, "Latency"),
                        e("th", null, "Show"),
                    )
                ),
                e("tbody", null, 
                    curr.map((it, ix) => 
                        e("tr", { key: ix },
                            e("td", null, ix+1),
                            e("td", null, 
                                it["dcs"].sort().join(", ")
                            ),
                            e("td", null, it["95th"]),
                            e("td", null, 
                                e("input", { 
                                    type: "radio", 
                                    value: ix,
                                    checked: selectedIx == ix,
                                    onChange: changed,
                                })
                            ),
                        )
                    )
                )
            )
        ), 
        e("div", { className: "pure-u-1" + (props.size == 1 ? " pure-u-lg-2-3" : "") },
            e("figure", null,
                e("img", { className: "pure-img", src: selectedSrc, key: selectedSrc })
            )
        )
    );
}

function AllMultipleDCRankings() {
    const firstIx = 2;

    const [selectedIx, setSelectedIx] = React.useState(firstIx);

    function handle_change(event) {
        setSelectedIx(event.target.value);
    }

    return e("div", null, 
        e("form", { className: "pure-form" },
            e("fieldset", { className: "pure-group" }, 
                e("select", { defaultValue: selectedIx, style: { width: "100%" }, onChange: handle_change}, 
                    [...LATENCY_MAPS.keys()].filter(i => i >= 2).map((ix) => 
                        e("option", { key: ix, value: ix }, `${ix} regions`),
                    )
                )
            ),
            e(DCRanking, { size: selectedIx })
        )
    )
}

ReactDOM.createRoot(document.getElementById("dc_ranking_one")).render(e(DCRanking, { size: 1 }, null));
ReactDOM.createRoot(document.getElementById("dc_ranking_all")).render(e(AllMultipleDCRankings, null, null));