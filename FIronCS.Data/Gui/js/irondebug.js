if (typeof iron == 'undefined') {
    iron = function () {

        var allRes = [[1024, 768],
                     [1280, 960],
                     [1400, 1050],
                     [1600, 1200]].sort();

        var video = {
            resolution: [1024, 768],
            fullscreen: false
        };

        var charTemplate = {
            name: "Name Me",
            gender: "Male",
            race: "Human",
            stats: {
                str: 10,
                dex: 10,
                con: 10,
                int: 10,
                pow: 10,
                siz: 10
            }
        }


        return {
            getVideo: function () { return video; },
            setVideo: function(s) { alert ("[DEBUG] video set!\n" + s.resolution + "\n" + s.fullscreen);},
            getResolutions: function () { return allRes; },
            clear: function () { },
            drawChar: function(r) { },
            charTemplate: function () { return charTemplate; }
        };
    }();
}