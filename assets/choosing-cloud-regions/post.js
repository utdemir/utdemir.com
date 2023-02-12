const e = React.createElement

const ASSET_ROOT = "/assets/choosing-cloud-regions";

const LATENCY_MAPS = [[], [{"dcs": ["eu-west-3"], "cost": 34.992, "fname": "eu-west-3", "median_latency": 163, "95th_latency": 266}, {"dcs": ["eu-west-2"], "cost": 33.5232, "fname": "eu-west-2", "median_latency": 153, "95th_latency": 270}, {"dcs": ["us-east-1"], "cost": 29.1456, "fname": "us-east-1", "median_latency": 214, "95th_latency": 271}, {"dcs": ["ca-central-1"], "cost": 32.083200000000005, "fname": "ca-central-1", "median_latency": 211, "95th_latency": 276}, {"dcs": ["us-west-2"], "cost": 29.1456, "fname": "us-west-2", "median_latency": 189, "95th_latency": 283}, {"dcs": ["us-west-1"], "cost": 33.5232, "fname": "us-west-1", "median_latency": 199, "95th_latency": 284}, {"dcs": ["eu-central-1"], "cost": 33.5232, "fname": "eu-central-1", "median_latency": 160, "95th_latency": 286}, {"dcs": ["me-south-1"], "cost": 37.872, "fname": "me-south-1", "median_latency": 176, "95th_latency": 295}, {"dcs": ["eu-west-1"], "cost": 29.1456, "fname": "eu-west-1", "median_latency": 168, "95th_latency": 299}, {"dcs": ["eu-south-1"], "cost": 34.992, "fname": "eu-south-1", "median_latency": 175, "95th_latency": 302}, {"dcs": ["ap-northeast-3"], "cost": 36.403200000000005, "fname": "ap-northeast-3", "median_latency": 157, "95th_latency": 324}, {"dcs": ["ap-south-1"], "cost": 30.643200000000004, "fname": "ap-south-1", "median_latency": 197, "95th_latency": 327}, {"dcs": ["eu-north-1"], "cost": 32.04, "fname": "eu-north-1", "median_latency": 184, "95th_latency": 333}, {"dcs": ["ap-east-1"], "cost": 40.032000000000004, "fname": "ap-east-1", "median_latency": 232, "95th_latency": 358}, {"dcs": ["ap-northeast-1"], "cost": 36.403200000000005, "fname": "ap-northeast-1", "median_latency": 204, "95th_latency": 363}], [{"dcs": ["ap-northeast-3", "eu-west-2"], "cost": 34.9632, "fname": "ap-northeast-3_eu-west-2", "median_latency": 92, "95th_latency": 177}, {"dcs": ["ap-northeast-3", "eu-west-1"], "cost": 32.7744, "fname": "ap-northeast-3_eu-west-1", "median_latency": 97, "95th_latency": 190}, {"dcs": ["ap-northeast-3", "eu-south-1"], "cost": 35.6976, "fname": "ap-northeast-3_eu-south-1", "median_latency": 100, "95th_latency": 190}, {"dcs": ["ap-northeast-3", "eu-west-3"], "cost": 35.6976, "fname": "ap-northeast-3_eu-west-3", "median_latency": 93, "95th_latency": 196}, {"dcs": ["ap-southeast-3", "eu-west-1"], "cost": 32.7744, "fname": "ap-southeast-3_eu-west-1", "median_latency": 97, "95th_latency": 199}, {"dcs": ["ap-northeast-3", "eu-central-1"], "cost": 34.9632, "fname": "ap-northeast-3_eu-central-1", "median_latency": 96, "95th_latency": 200}, {"dcs": ["ap-northeast-1", "eu-west-1"], "cost": 32.7744, "fname": "ap-northeast-1_eu-west-1", "median_latency": 117, "95th_latency": 202}, {"dcs": ["eu-west-3", "ap-southeast-3"], "cost": 35.6976, "fname": "eu-west-3_ap-southeast-3", "median_latency": 93, "95th_latency": 203}, {"dcs": ["eu-west-2", "ap-northeast-1"], "cost": 34.9632, "fname": "eu-west-2_ap-northeast-1", "median_latency": 111, "95th_latency": 203}, {"dcs": ["ap-northeast-3", "ca-central-1"], "cost": 34.2432, "fname": "ap-northeast-3_ca-central-1", "median_latency": 105, "95th_latency": 207}, {"dcs": ["eu-central-1", "ap-southeast-3"], "cost": 34.9632, "fname": "eu-central-1_ap-southeast-3", "median_latency": 96, "95th_latency": 209}, {"dcs": ["us-west-2", "eu-west-1"], "cost": 29.1456, "fname": "us-west-2_eu-west-1", "median_latency": 157, "95th_latency": 209}, {"dcs": ["eu-south-1", "ap-southeast-3"], "cost": 35.6976, "fname": "eu-south-1_ap-southeast-3", "median_latency": 100, "95th_latency": 210}, {"dcs": ["us-west-2", "eu-north-1"], "cost": 30.5928, "fname": "us-west-2_eu-north-1", "median_latency": 164, "95th_latency": 210}, {"dcs": ["us-west-2", "eu-west-3"], "cost": 32.068799999999996, "fname": "us-west-2_eu-west-3", "median_latency": 142, "95th_latency": 211}], [{"dcs": ["ap-northeast-3", "ca-central-1", "eu-west-2"], "cost": 34.0032, "fname": "ap-northeast-3_ca-central-1_eu-west-2", "median_latency": 84, "95th_latency": 156}, {"dcs": ["ap-northeast-3", "eu-west-2", "us-east-2"], "cost": 33.024, "fname": "ap-northeast-3_eu-west-2_us-east-2", "median_latency": 84, "95th_latency": 156}, {"dcs": ["ap-northeast-3", "eu-west-2", "sa-east-1"], "cost": 40.0128, "fname": "ap-northeast-3_eu-west-2_sa-east-1", "median_latency": 88, "95th_latency": 156}, {"dcs": ["ap-northeast-3", "eu-west-2", "us-east-1"], "cost": 33.024, "fname": "ap-northeast-3_eu-west-2_us-east-1", "median_latency": 84, "95th_latency": 157}, {"dcs": ["ap-northeast-3", "ca-central-1", "eu-west-3"], "cost": 34.492799999999995, "fname": "ap-northeast-3_ca-central-1_eu-west-3", "median_latency": 85, "95th_latency": 158}, {"dcs": ["ap-northeast-3", "us-east-2", "eu-west-3"], "cost": 33.5136, "fname": "ap-northeast-3_us-east-2_eu-west-3", "median_latency": 85, "95th_latency": 158}, {"dcs": ["ap-northeast-3", "us-east-1", "eu-west-3"], "cost": 33.5136, "fname": "ap-northeast-3_us-east-1_eu-west-3", "median_latency": 84, "95th_latency": 159}, {"dcs": ["ap-northeast-3", "eu-west-3", "sa-east-1"], "cost": 40.5024, "fname": "ap-northeast-3_eu-west-3_sa-east-1", "median_latency": 89, "95th_latency": 159}, {"dcs": ["ap-northeast-3", "us-east-2", "eu-south-1"], "cost": 33.5136, "fname": "ap-northeast-3_us-east-2_eu-south-1", "median_latency": 85, "95th_latency": 161}, {"dcs": ["ap-northeast-3", "us-east-2", "eu-central-1"], "cost": 33.024, "fname": "ap-northeast-3_us-east-2_eu-central-1", "median_latency": 84, "95th_latency": 162}, {"dcs": ["ap-northeast-3", "ca-central-1", "eu-south-1"], "cost": 34.492799999999995, "fname": "ap-northeast-3_ca-central-1_eu-south-1", "median_latency": 85, "95th_latency": 162}, {"dcs": ["ap-northeast-3", "us-east-2", "eu-west-1"], "cost": 31.5648, "fname": "ap-northeast-3_us-east-2_eu-west-1", "median_latency": 86, "95th_latency": 162}, {"dcs": ["ap-northeast-3", "sa-east-1", "eu-west-1"], "cost": 38.553599999999996, "fname": "ap-northeast-3_sa-east-1_eu-west-1", "median_latency": 92, "95th_latency": 162}, {"dcs": ["ap-northeast-3", "us-east-1", "eu-south-1"], "cost": 33.5136, "fname": "ap-northeast-3_us-east-1_eu-south-1", "median_latency": 84, "95th_latency": 163}, {"dcs": ["ap-northeast-3", "ca-central-1", "eu-west-1"], "cost": 32.544000000000004, "fname": "ap-northeast-3_ca-central-1_eu-west-1", "median_latency": 86, "95th_latency": 163}], [{"dcs": ["ap-northeast-3", "ca-central-1", "eu-south-1", "ap-southeast-3"], "cost": 34.9704, "fname": "ap-northeast-3_ca-central-1_eu-south-1_ap-southeast-3", "median_latency": 72, "95th_latency": 141}, {"dcs": ["ap-northeast-3", "ca-central-1", "eu-west-3", "ap-southeast-1"], "cost": 34.9704, "fname": "ap-northeast-3_ca-central-1_eu-west-3_ap-southeast-1", "median_latency": 65, "95th_latency": 142}, {"dcs": ["ap-northeast-3", "ca-central-1", "eu-west-3", "ap-southeast-3"], "cost": 34.9704, "fname": "ap-northeast-3_ca-central-1_eu-west-3_ap-southeast-3", "median_latency": 71, "95th_latency": 142}, {"dcs": ["ap-northeast-3", "us-east-1", "eu-south-1", "ap-southeast-3"], "cost": 34.236, "fname": "ap-northeast-3_us-east-1_eu-south-1_ap-southeast-3", "median_latency": 71, "95th_latency": 142}, {"dcs": ["ap-northeast-3", "ca-central-1", "ap-southeast-3", "eu-west-1"], "cost": 33.5088, "fname": "ap-northeast-3_ca-central-1_ap-southeast-3_eu-west-1", "median_latency": 73, "95th_latency": 142}, {"dcs": ["ap-northeast-3", "us-east-1", "eu-west-3", "ap-southeast-1"], "cost": 34.236, "fname": "ap-northeast-3_us-east-1_eu-west-3_ap-southeast-1", "median_latency": 65, "95th_latency": 143}, {"dcs": ["ap-northeast-3", "ca-central-1", "eu-west-2", "ap-southeast-3"], "cost": 34.6032, "fname": "ap-northeast-3_ca-central-1_eu-west-2_ap-southeast-3", "median_latency": 71, "95th_latency": 143}, {"dcs": ["ap-northeast-3", "ca-central-1", "eu-west-3", "ap-south-1"], "cost": 33.5304, "fname": "ap-northeast-3_ca-central-1_eu-west-3_ap-south-1", "median_latency": 68, "95th_latency": 144}, {"dcs": ["ap-northeast-3", "us-east-1", "eu-west-3", "ap-southeast-3"], "cost": 34.236, "fname": "ap-northeast-3_us-east-1_eu-west-3_ap-southeast-3", "median_latency": 70, "95th_latency": 144}, {"dcs": ["ap-northeast-3", "us-east-2", "eu-south-1", "ap-southeast-3"], "cost": 34.236, "fname": "ap-northeast-3_us-east-2_eu-south-1_ap-southeast-3", "median_latency": 71, "95th_latency": 144}, {"dcs": ["ap-northeast-3", "us-east-1", "ap-southeast-3", "eu-west-1"], "cost": 32.7744, "fname": "ap-northeast-3_us-east-1_ap-southeast-3_eu-west-1", "median_latency": 72, "95th_latency": 144}, {"dcs": ["ap-northeast-3", "us-east-1", "eu-west-3", "ap-south-1"], "cost": 32.796, "fname": "ap-northeast-3_us-east-1_eu-west-3_ap-south-1", "median_latency": 67, "95th_latency": 145}, {"dcs": ["ap-northeast-3", "eu-west-2", "us-east-1", "ap-southeast-3"], "cost": 33.8688, "fname": "ap-northeast-3_eu-west-2_us-east-1_ap-southeast-3", "median_latency": 70, "95th_latency": 145}, {"dcs": ["ap-northeast-3", "ca-central-1", "eu-west-2", "ap-south-1"], "cost": 33.1632, "fname": "ap-northeast-3_ca-central-1_eu-west-2_ap-south-1", "median_latency": 68, "95th_latency": 146}, {"dcs": ["ap-northeast-3", "eu-west-2", "us-east-2", "ap-southeast-3"], "cost": 33.8688, "fname": "ap-northeast-3_eu-west-2_us-east-2_ap-southeast-3", "median_latency": 70, "95th_latency": 146}], [{"dcs": ["ap-northeast-3", "af-south-1", "us-east-1", "eu-central-1", "ap-southeast-3"], "cost": 34.957440000000005, "fname": "ap-northeast-3_af-south-1_us-east-1_eu-central-1_ap-southeast-3", "median_latency": 68, "95th_latency": 124}, {"dcs": ["ap-northeast-3", "af-south-1", "us-east-1", "eu-west-3", "ap-southeast-1"], "cost": 35.2512, "fname": "ap-northeast-3_af-south-1_us-east-1_eu-west-3_ap-southeast-1", "median_latency": 64, "95th_latency": 125}, {"dcs": ["ap-northeast-3", "af-south-1", "us-east-1", "eu-west-3", "ap-southeast-3"], "cost": 35.2512, "fname": "ap-northeast-3_af-south-1_us-east-1_eu-west-3_ap-southeast-3", "median_latency": 69, "95th_latency": 125}, {"dcs": ["ap-northeast-3", "eu-west-2", "af-south-1", "us-east-1", "ap-southeast-3"], "cost": 34.957440000000005, "fname": "ap-northeast-3_eu-west-2_af-south-1_us-east-1_ap-southeast-3", "median_latency": 68, "95th_latency": 126}, {"dcs": ["ap-northeast-3", "ca-central-1", "eu-west-3", "sa-east-1", "ap-southeast-1"], "cost": 37.99872, "fname": "ap-northeast-3_ca-central-1_eu-west-3_sa-east-1_ap-southeast-1", "median_latency": 62, "95th_latency": 127}, {"dcs": ["ap-northeast-3", "us-east-2", "eu-west-3", "sa-east-1", "ap-southeast-1"], "cost": 37.411199999999994, "fname": "ap-northeast-3_us-east-2_eu-west-3_sa-east-1_ap-southeast-1", "median_latency": 62, "95th_latency": 127}, {"dcs": ["ap-northeast-3", "us-east-1", "eu-west-3", "sa-east-1", "ap-southeast-1"], "cost": 37.411199999999994, "fname": "ap-northeast-3_us-east-1_eu-west-3_sa-east-1_ap-southeast-1", "median_latency": 62, "95th_latency": 127}, {"dcs": ["ap-northeast-3", "us-east-2", "eu-west-3", "sa-east-1", "ap-southeast-3"], "cost": 37.411199999999994, "fname": "ap-northeast-3_us-east-2_eu-west-3_sa-east-1_ap-southeast-3", "median_latency": 66, "95th_latency": 127}, {"dcs": ["ap-northeast-3", "ca-central-1", "eu-west-3", "sa-east-1", "ap-southeast-3"], "cost": 37.99872, "fname": "ap-northeast-3_ca-central-1_eu-west-3_sa-east-1_ap-southeast-3", "median_latency": 67, "95th_latency": 127}, {"dcs": ["ap-northeast-3", "us-east-1", "eu-west-3", "sa-east-1", "ap-southeast-3"], "cost": 37.411199999999994, "fname": "ap-northeast-3_us-east-1_eu-west-3_sa-east-1_ap-southeast-3", "median_latency": 67, "95th_latency": 127}, {"dcs": ["ap-northeast-3", "af-south-1", "us-east-1", "eu-south-1", "ap-southeast-3"], "cost": 35.2512, "fname": "ap-northeast-3_af-south-1_us-east-1_eu-south-1_ap-southeast-3", "median_latency": 70, "95th_latency": 127}, {"dcs": ["ap-northeast-3", "af-south-1", "us-east-1", "ap-southeast-3", "eu-north-1"], "cost": 34.6608, "fname": "ap-northeast-3_af-south-1_us-east-1_ap-southeast-3_eu-north-1", "median_latency": 71, "95th_latency": 127}, {"dcs": ["ap-northeast-3", "af-south-1", "us-east-1", "eu-central-1", "ap-southeast-1"], "cost": 34.957440000000005, "fname": "ap-northeast-3_af-south-1_us-east-1_eu-central-1_ap-southeast-1", "median_latency": 63, "95th_latency": 128}, {"dcs": ["ap-northeast-3", "ca-central-1", "eu-west-2", "sa-east-1", "ap-southeast-3"], "cost": 37.70496, "fname": "ap-northeast-3_ca-central-1_eu-west-2_sa-east-1_ap-southeast-3", "median_latency": 67, "95th_latency": 128}, {"dcs": ["ap-northeast-3", "eu-west-2", "us-east-1", "sa-east-1", "ap-southeast-3"], "cost": 37.11744, "fname": "ap-northeast-3_eu-west-2_us-east-1_sa-east-1_ap-southeast-3", "median_latency": 67, "95th_latency": 128}], [{"dcs": ["ap-northeast-3", "ca-central-1", "af-south-1", "eu-central-1", "sa-east-1", "ap-southeast-1"], "cost": 37.9728, "fname": "ap-northeast-3_ca-central-1_af-south-1_eu-central-1_sa-east-1_ap-southeast-1", "median_latency": 59, "95th_latency": 114}, {"dcs": ["ap-northeast-3", "us-east-2", "af-south-1", "eu-central-1", "sa-east-1", "ap-southeast-1"], "cost": 37.483200000000004, "fname": "ap-northeast-3_us-east-2_af-south-1_eu-central-1_sa-east-1_ap-southeast-1", "median_latency": 59, "95th_latency": 114}, {"dcs": ["ap-northeast-3", "af-south-1", "us-east-1", "eu-central-1", "sa-east-1", "ap-southeast-1"], "cost": 37.483200000000004, "fname": "ap-northeast-3_af-south-1_us-east-1_eu-central-1_sa-east-1_ap-southeast-1", "median_latency": 60, "95th_latency": 114}, {"dcs": ["ap-northeast-3", "ca-central-1", "af-south-1", "eu-west-3", "sa-east-1", "ap-southeast-1"], "cost": 38.2176, "fname": "ap-northeast-3_ca-central-1_af-south-1_eu-west-3_sa-east-1_ap-southeast-1", "median_latency": 60, "95th_latency": 115}, {"dcs": ["ap-northeast-3", "us-east-2", "af-south-1", "eu-west-3", "sa-east-1", "ap-southeast-1"], "cost": 37.728, "fname": "ap-northeast-3_us-east-2_af-south-1_eu-west-3_sa-east-1_ap-southeast-1", "median_latency": 60, "95th_latency": 115}, {"dcs": ["ap-northeast-3", "af-south-1", "us-east-1", "eu-west-3", "sa-east-1", "ap-southeast-1"], "cost": 37.728, "fname": "ap-northeast-3_af-south-1_us-east-1_eu-west-3_sa-east-1_ap-southeast-1", "median_latency": 61, "95th_latency": 115}, {"dcs": ["ap-northeast-3", "ca-central-1", "af-south-1", "eu-central-1", "sa-east-1", "ap-southeast-3"], "cost": 37.9728, "fname": "ap-northeast-3_ca-central-1_af-south-1_eu-central-1_sa-east-1_ap-southeast-3", "median_latency": 64, "95th_latency": 117}, {"dcs": ["ap-northeast-3", "us-east-2", "af-south-1", "eu-central-1", "sa-east-1", "ap-southeast-3"], "cost": 37.483200000000004, "fname": "ap-northeast-3_us-east-2_af-south-1_eu-central-1_sa-east-1_ap-southeast-3", "median_latency": 64, "95th_latency": 117}, {"dcs": ["ap-northeast-3", "af-south-1", "us-east-1", "eu-central-1", "sa-east-1", "ap-southeast-3"], "cost": 37.483200000000004, "fname": "ap-northeast-3_af-south-1_us-east-1_eu-central-1_sa-east-1_ap-southeast-3", "median_latency": 64, "95th_latency": 117}, {"dcs": ["ap-northeast-3", "ca-central-1", "af-south-1", "eu-west-3", "sa-east-1", "ap-southeast-3"], "cost": 38.2176, "fname": "ap-northeast-3_ca-central-1_af-south-1_eu-west-3_sa-east-1_ap-southeast-3", "median_latency": 66, "95th_latency": 117}, {"dcs": ["ap-northeast-3", "ca-central-1", "eu-west-2", "af-south-1", "sa-east-1", "ap-southeast-1"], "cost": 37.9728, "fname": "ap-northeast-3_ca-central-1_eu-west-2_af-south-1_sa-east-1_ap-southeast-1", "median_latency": 61, "95th_latency": 118}, {"dcs": ["ap-northeast-3", "ca-central-1", "eu-west-2", "sa-east-1", "me-south-1", "ap-southeast-1"], "cost": 37.732800000000005, "fname": "ap-northeast-3_ca-central-1_eu-west-2_sa-east-1_me-south-1_ap-southeast-1", "median_latency": 61, "95th_latency": 118}, {"dcs": ["ap-northeast-3", "eu-west-2", "us-east-2", "af-south-1", "sa-east-1", "ap-southeast-1"], "cost": 37.483200000000004, "fname": "ap-northeast-3_eu-west-2_us-east-2_af-south-1_sa-east-1_ap-southeast-1", "median_latency": 61, "95th_latency": 118}, {"dcs": ["ap-northeast-3", "eu-west-2", "us-east-2", "sa-east-1", "me-south-1", "ap-southeast-1"], "cost": 37.243199999999995, "fname": "ap-northeast-3_eu-west-2_us-east-2_sa-east-1_me-south-1_ap-southeast-1", "median_latency": 61, "95th_latency": 118}, {"dcs": ["ap-northeast-3", "eu-west-2", "af-south-1", "us-east-1", "sa-east-1", "ap-southeast-1"], "cost": 37.483200000000004, "fname": "ap-northeast-3_eu-west-2_af-south-1_us-east-1_sa-east-1_ap-southeast-1", "median_latency": 61, "95th_latency": 118}]]

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
                e("img", { src: ASSET_ROOT + "/" + obj.fname + ".png" }, null),
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

    const selectedSrc = ASSET_ROOT + "/lm/" + curr[selectedIx].fname + ".png"

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
                    curr.slice(0, 10).map((it, ix) => 
                        e("tr", { key: ix },
                            e("td", null, ix+1),
                            e("td", null, 
                                it["dcs"].sort().join(", ")
                            ),
                            e("td", null, it["95th_latency"]),
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