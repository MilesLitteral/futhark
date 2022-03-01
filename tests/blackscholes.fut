-- --
-- input {
--   5
-- }
-- output {
--   [0.000000, 0.000000, 0.000006, 0.000077, 0.000386, 0.001183, 0.002707, 0.005143,
--    0.008608, 0.013162, 0.018818, 0.025560, 0.033350, 0.042137, 0.051866, 0.062475,
--    0.073906, 0.086099, 0.098998, 0.112550, 0.126705, 0.141416, 0.156641, 0.172339,
--    0.188473, 0.205010, 0.221917, 0.239166, 0.256731, 0.274587, 0.292711, 0.311083,
--    0.329684, 0.348495, 0.367501, 0.386687, 0.406039, 0.425543, 0.445188, 0.464964,
--    0.484859, 0.504864, 0.524971, 0.545171, 0.565457, 0.585822, 0.606259, 0.626761,
--    0.647324, 0.667942, 0.688610, 0.709324, 0.730078, 0.750870, 0.771694, 0.792548,
--    0.813429, 0.834333, 0.855257, 0.876199, 0.897156, 0.918125, 0.939105, 0.960093,
--    0.981087, 1.002086, 1.023088, 1.044090, 1.065092, 1.086092, 1.107089, 1.128081,
--    1.149067, 1.170047, 1.191018, 1.211981, 1.232933, 1.253875, 1.274805, 1.295722,
--    1.316626, 1.337517, 1.358392, 1.379253, 1.400098, 1.420927, 1.441739, 1.462533,
--    1.483310, 1.504069, 1.524810, 1.545531, 1.566233, 1.586916, 1.607579, 1.628222,
--    1.648844, 1.669446, 1.690027, 1.710587, 1.731126, 1.751643, 1.772139, 1.792613,
--    1.813065, 1.833496, 1.853904, 1.874289, 1.894653, 1.914994, 1.935313, 1.955609,
--    1.975882, 1.996133, 2.016361, 2.036566, 2.056749, 2.076909, 2.097046, 2.117160,
--    2.137251, 2.157319, 2.177364, 2.197387, 2.217387, 2.237364, 2.257318, 2.277249,
--    2.297157, 2.317043, 2.336906, 2.356746, 2.376564, 2.396359, 2.416131, 2.435881,
--    2.455608, 2.475313, 2.494996, 2.514656, 2.534294, 2.553910, 2.573503, 2.593075,
--    2.612624, 2.632152, 2.651657, 2.671141, 2.690602, 2.710042, 2.729461, 2.748857,
--    2.768233, 2.787586, 2.806919, 2.826230, 2.845520, 2.864788, 2.884036, 2.903262,
--    2.922468, 2.941652, 2.960816, 2.979959, 2.999081, 3.018183, 3.037265, 3.056325,
--    3.075366, 3.094386, 3.113386, 3.132366, 3.151325, 3.170265, 3.189185, 3.208085,
--    3.226965, 3.245826, 3.264667, 3.283488, 3.302290, 3.321073, 3.339836, 3.358580,
--    3.377305, 3.396011, 3.414697, 3.433365, 3.452014, 3.470644, 3.489256, 3.507849,
--    3.526423, 3.544979, 3.563516, 3.582035, 3.600536, 3.619019, 3.637483, 3.655930,
--    3.674358, 3.692769, 3.711162, 3.729537, 3.747894, 3.766233, 3.784556, 3.802860,
--    3.821147, 3.839417, 3.857670, 3.875905, 3.894123, 3.912325, 3.930509, 3.948676,
--    3.966826, 3.984960, 4.003077, 4.021177, 4.039260, 4.057327, 4.075378, 4.093412,
--    4.111430, 4.129431, 4.147416, 4.165386, 4.183339, 4.201276, 4.219197, 4.237102,
--    4.254991, 4.272864, 4.290722, 4.308564, 4.326391, 4.344202, 4.361997, 4.379777,
--    4.397542, 4.415291, 4.433025, 4.450744, 4.468448, 4.486137, 4.503811, 4.521469,
--    4.539113, 4.556742, 4.574357, 4.591956, 4.609541, 4.627111, 4.644667, 4.662208,
--    4.679735, 4.697247, 4.714745, 4.732228, 4.749698, 4.767153, 4.784594, 4.802021,
--    4.819433, 4.836832, 4.854217, 4.871588, 4.888946, 4.906289, 4.923619, 4.940935,
--    4.958237, 4.975526, 4.992801, 5.010063, 5.027311, 5.044546, 5.061767, 5.078975,
--    5.096170, 5.113352, 5.130521, 5.147676, 5.164818, 5.181948, 5.199064, 5.216168,
--    5.233258, 5.250336, 5.267401, 5.284453, 5.301492, 5.318519, 5.335533, 5.352535,
--    5.369524, 5.386500, 5.403464, 5.420416, 5.437355, 5.454282, 5.471197, 5.488099,
--    5.504989, 5.521867, 5.538733, 5.555587, 5.572428, 5.589258, 5.606076, 5.622881,
--    5.639675, 5.656457, 5.673227, 5.689986, 5.706732, 5.723467, 5.740190, 5.756902,
--    5.773602, 5.790290, 5.806967, 5.823633, 5.840287, 5.856929, 5.873560, 5.890180,
--    5.906789, 5.923386, 5.939972, 5.956547, 5.973111, 5.989663, 6.006205, 6.022735,
--    6.039254, 6.055763, 6.072260, 6.088747, 6.105223, 6.121687, 6.138141, 6.154585,
--    6.171017, 6.187439, 6.203850, 6.220250, 6.236640, 6.253019, 6.269387, 6.285746,
--    6.302093, 6.318430, 6.334757, 6.351073, 6.367379, 6.383674, 6.399960, 6.416235,
--    6.432499, 6.448754, 6.464998, 6.481232, 6.497456, 6.513670, 6.529874, 6.546067,
--    6.562251, 6.578425, 6.594589, 6.610742, 6.626886, 6.643020, 6.659145, 6.675259,
--    6.691363, 6.707458, 6.723543, 6.739619, 6.755684, 6.771740, 6.787787, 6.803824,
--    6.819851, 6.835869, 6.851877, 6.867875, 6.883865, 6.899844, 6.915815, 6.931776,
--    6.947727, 6.963669, 6.979602, 6.995526, 7.011440, 7.027345, 7.043241, 7.059128,
--    7.075005, 7.090874, 7.106733, 7.122583, 7.138424, 7.154256, 7.170079, 7.185893,
--    7.201698, 7.217494, 7.233281, 7.249060, 7.264829, 7.280589, 7.296341, 7.312084,
--    7.327818, 7.343543, 7.359260, 7.374968, 7.390667, 7.406357, 7.422039, 7.437712,
--    7.453376, 7.469032, 7.484680, 7.500318, 7.515949, 7.531570, 7.547184, 7.562789,
--    7.578385, 7.593973, 7.609552, 7.625124, 7.640686, 7.656241, 7.671787, 7.687325,
--    7.702854, 7.718376, 7.733889, 7.749393, 7.764890, 7.780378, 7.795859, 7.811331,
--    7.826795, 7.842251, 7.857699, 7.873138, 7.888570, 7.903994, 7.919410, 7.934817,
--    7.950217, 7.965609, 7.980993, 7.996369, 8.011737, 8.027097, 8.042449, 8.057794,
--    8.073130, 8.088459, 8.103780, 8.119094, 8.134399, 8.149697, 8.164987, 8.180270,
--    8.195544, 8.210811, 8.226071, 8.241323, 8.256567, 8.271803, 8.287032, 8.302254,
--    8.317468, 8.332674, 8.347873, 8.363064, 8.378248, 8.393425, 8.408594, 8.423755,
--    8.438910, 8.454056, 8.469196, 8.484328, 8.499453, 8.514570, 8.529680, 8.544783,
--    8.559878, 8.574966, 8.590047, 8.605121, 8.620188, 8.635247, 8.650299, 8.665344,
--    8.680382, 8.695412, 8.710436, 8.725452, 8.740462, 8.755464, 8.770459, 8.785447,
--    8.800428, 8.815402, 8.830369, 8.845329, 8.860283, 8.875229, 8.890168, 8.905100,
--    8.920025, 8.934944, 8.949855, 8.964760, 8.979658, 8.994548, 9.009432, 9.024310,
--    9.039180, 9.054044, 9.068901, 9.083751, 9.098594, 9.113431, 9.128260, 9.143084,
--    9.157900, 9.172710, 9.187513, 9.202309, 9.217099, 9.231882, 9.246659, 9.261428,
--    9.276192, 9.290948, 9.305699, 9.320442, 9.335179, 9.349910, 9.364634, 9.379351,
--    9.394062, 9.408767, 9.423465, 9.438156, 9.452841, 9.467520, 9.482192, 9.496858,
--    9.511518, 9.526171, 9.540817, 9.555458, 9.570092, 9.584719, 9.599341, 9.613956,
--    9.628565, 9.643167, 9.657763, 9.672353, 9.686937, 9.701514, 9.716085, 9.730650,
--    9.745209, 9.759761, 9.774308, 9.788848, 9.803382, 9.817910, 9.832432, 9.846947,
--    9.861457, 9.875960, 9.890457, 9.904949, 9.919434, 9.933913, 9.948386, 9.962853,
--    9.977314, 9.991769, 10.006218, 10.020661, 10.035097, 10.049528, 10.063953,
--    10.078372, 10.092786, 10.107193, 10.121594, 10.135989, 10.150379, 10.164762,
--    10.179140, 10.193512, 10.207877, 10.222238, 10.236592, 10.250940, 10.265283,
--    10.279619, 10.293950, 10.308275, 10.322595, 10.336908, 10.351216, 10.365518,
--    10.379815, 10.394105, 10.408390, 10.422669, 10.436943, 10.451210, 10.465473,
--    10.479729, 10.493980, 10.508225, 10.522464, 10.536698, 10.550926, 10.565149,
--    10.579366, 10.593577, 10.607783, 10.621983, 10.636177, 10.650366, 10.664550,
--    10.678728, 10.692900, 10.707067, 10.721228, 10.735384, 10.749534, 10.763679,
--    10.777819, 10.791952, 10.806081, 10.820204, 10.834321, 10.848433, 10.862540,
--    10.876641, 10.890737, 10.904827, 10.918912, 10.932992, 10.947066, 10.961135,
--    10.975198, 10.989257, 11.003309, 11.017357, 11.031399, 11.045436, 11.059467,
--    11.073493, 11.087514, 11.101530, 11.115540, 11.129545, 11.143545, 11.157539,
--    11.171529, 11.185513, 11.199492, 11.213465, 11.227434, 11.241397, 11.255355,
--    11.269307, 11.283255, 11.297197, 11.311135, 11.325067, 11.338994, 11.352915,
--    11.366832, 11.380744, 11.394650, 11.408551, 11.422447, 11.436338, 11.450224,
--    11.464105, 11.477981, 11.491852, 11.505718, 11.519578, 11.533434, 11.547284,
--    11.561130, 11.574970, 11.588806, 11.602636, 11.616462, 11.630282, 11.644098,
--    11.657908, 11.671714, 11.685514, 11.699310, 11.713101, 11.726886, 11.740667,
--    11.754443, 11.768214, 11.781980, 11.795741, 11.809497, 11.823248, 11.836995,
--    11.850736, 11.864473, 11.878205, 11.891932, 11.905654, 11.919371, 11.933083,
--    11.946791, 11.960493, 11.974191, 11.987884, 12.001573, 12.015256, 12.028935,
--    12.042609, 12.056278, 12.069942, 12.083602, 12.097256, 12.110907, 12.124552,
--    12.138192, 12.151828, 12.165459, 12.179086, 12.192707, 12.206324, 12.219936,
--    12.233544, 12.247147, 12.260745, 12.274338, 12.287927, 12.301511, 12.315091,
--    12.328666, 12.342236, 12.355801, 12.369362, 12.382919, 12.396470, 12.410017,
--    12.423560, 12.437098, 12.450631, 12.464159, 12.477684, 12.491203, 12.504718,
--    12.518228, 12.531734, 12.545235, 12.558732, 12.572224, 12.585712, 12.599195,
--    12.612673, 12.626147, 12.639617, 12.653082, 12.666542, 12.679998, 12.693450,
--    12.706897, 12.720339, 12.733777, 12.747211, 12.760640, 12.774064, 12.787485,
--    12.800900, 12.814312, 12.827719, 12.841121, 12.854519, 12.867913, 12.881302,
--    12.894687, 12.908067, 12.921443, 12.934815, 12.948182, 12.961545, 12.974903,
--    12.988257, 13.001607, 13.014952, 13.028293, 13.041630, 13.054962, 13.068290,
--    13.081614, 13.094933, 13.108248, 13.121559, 13.134865, 13.148167, 13.161465,
--    13.174759, 13.188048, 13.201333, 13.214613, 13.227890, 13.241162, 13.254430,
--    13.267693, 13.280952, 13.294207, 13.307458, 13.320705, 13.333947, 13.347185,
--    13.360419, 13.373649, 13.386874, 13.400095, 13.413313, 13.426525, 13.439734,
--    13.452938, 13.466139, 13.479335, 13.492527, 13.505714, 13.518898, 13.532077,
--    13.545253, 13.558424, 13.571591, 13.584754, 13.597912, 13.611067, 13.624217,
--    13.637364, 13.650506, 13.663644, 13.676778, 13.689908, 13.703034, 13.716155,
--    13.729273, 13.742386, 13.755496, 13.768601, 13.781703, 13.794800, 13.807893,
--    13.820982, 13.834067, 13.847148, 13.860225, 13.873298, 13.886367, 13.899432,
--    13.912493, 13.925550, 13.938602, 13.951651, 13.964696, 13.977737, 13.990774,
--    14.003806, 14.016835, 14.029860, 14.042881, 14.055898, 14.068911, 14.081920,
--    14.094925, 14.107926, 14.120923, 14.133916, 14.146905, 14.159890, 14.172872,
--    14.185849, 14.198822, 14.211792, 14.224758, 14.237719, 14.250677, 14.263631,
--    14.276581, 14.289527, 14.302469, 14.315407, 14.328342, 14.341272, 14.354199,
--    14.367122, 14.380041, 14.392956, 14.405867, 14.418774, 14.431677, 14.444577,
--    14.457473, 14.470365, 14.483253, 14.496137, 14.509017, 14.521894, 14.534767,
--    14.547636, 14.560501, 14.573362, 14.586220, 14.599074, 14.611923, 14.624770,
--    14.637612, 14.650450, 14.663285, 14.676116, 14.688943, 14.701767, 14.714587,
--    14.727403, 14.740215, 14.753023, 14.765828, 14.778629, 14.791426, 14.804219,
--    14.817009, 14.829795, 14.842577, 14.855356, 14.868130, 14.880902, 14.893669,
--    14.906433, 14.919192, 14.931949, 14.944701, 14.957450, 14.970195, 14.982937,
--    14.995674, 15.008409, 15.021139, 15.033866, 15.046589, 15.059308, 15.072024,
--    15.084736, 15.097445, 15.110149, 15.122851, 15.135548, 15.148242, 15.160932,
--    15.173619, 15.186302, 15.198981, 15.211657, 15.224329, 15.236997, 15.249662,
--    15.262323, 15.274981, 15.287635, 15.300286, 15.312932, 15.325576, 15.338215,
--    15.350851, 15.363484, 15.376113, 15.388738, 15.401360, 15.413978, 15.426593,
--    15.439204, 15.451812, 15.464416, 15.477016, 15.489613, 15.502206, 15.514796,
--    15.527382, 15.539965, 15.552544, 15.565120, 15.577692, 15.590261, 15.602826,
--    15.615387, 15.627945, 15.640500, 15.653051, 15.665599, 15.678143, 15.690683,
--    15.703220, 15.715754, 15.728284, 15.740811, 15.753334, 15.765854, 15.778370,
--    15.790883, 15.803392, 15.815898, 15.828400, 15.840899, 15.853394, 15.865886,
--    15.878375, 15.890860, 15.903342, 15.915820, 15.928295, 15.940766, 15.953234,
--    15.965698, 15.978159, 15.990617, 16.003071, 16.015522, 16.027970, 16.040414,
--    16.052854, 16.065292, 16.077725, 16.090156, 16.102583, 16.115006, 16.127427,
--    16.139844, 16.152257, 16.164667, 16.177074, 16.189477, 16.201877, 16.214274,
--    16.226667, 16.239057, 16.251444, 16.263827, 16.276207, 16.288583, 16.300956,
--    16.313326, 16.325693, 16.338056, 16.350416, 16.362772, 16.375125, 16.387475,
--    16.399822, 16.412165, 16.424505, 16.436841, 16.449175, 16.461505, 16.473831,
--    16.486155, 16.498475, 16.510791, 16.523105, 16.535415, 16.547722, 16.560026,
--    16.572326, 16.584623, 16.596917, 16.609207, 16.621494, 16.633778, 16.646059,
--    16.658337, 16.670611, 16.682882, 16.695149, 16.707414, 16.719675, 16.731933,
--    16.744187, 16.756439, 16.768687, 16.780932, 16.793174, 16.805412, 16.817647,
--    16.829880, 16.842108, 16.854334, 16.866556, 16.878776, 16.890991, 16.903204,
--    16.915414, 16.927620, 16.939823, 16.952023, 16.964220, 16.976414, 16.988604,
--    17.000791, 17.012975, 17.025156, 17.037334, 17.049508, 17.061679, 17.073847,
--    17.086012, 17.098174, 17.110333, 17.122488, 17.134640, 17.146790, 17.158936,
--    17.171078, 17.183218, 17.195355, 17.207488, 17.219618, 17.231745, 17.243869,
--    17.255990, 17.268108, 17.280222, 17.292334, 17.304442, 17.316547, 17.328649,
--    17.340748, 17.352844, 17.364937, 17.377027, 17.389113, 17.401197, 17.413277,
--    17.425354, 17.437428, 17.449499, 17.461567, 17.473632, 17.485694, 17.497752,
--    17.509808, 17.521860, 17.533910, 17.545956, 17.558000, 17.570040, 17.582077,
--    17.594111, 17.606142, 17.618170, 17.630195, 17.642217, 17.654235, 17.666251,
--    17.678264, 17.690273, 17.702280, 17.714284, 17.726284, 17.738282, 17.750276,
--    17.762267, 17.774256, 17.786241, 17.798223, 17.810203, 17.822179, 17.834152,
--    17.846122, 17.858090, 17.870054, 17.882015, 17.893973, 17.905928, 17.917880,
--    17.929830, 17.941776, 17.953719, 17.965659, 17.977596, 17.989530, 18.001462,
--    18.013390, 18.025315, 18.037237, 18.049157, 18.061073, 18.072986, 18.084897,
--    18.096804, 18.108708, 18.120610, 18.132508, 18.144404, 18.156296, 18.168186,
--    18.180073, 18.191956, 18.203837, 18.215715, 18.227590, 18.239462, 18.251330,
--    18.263196, 18.275060, 18.286920, 18.298777, 18.310631, 18.322483, 18.334331,
--    18.346176, 18.358019, 18.369859, 18.381695, 18.393529, 18.405360, 18.417188,
--    18.429013, 18.440835, 18.452655, 18.464471, 18.476284, 18.488095, 18.499902,
--    18.511707, 18.523509, 18.535308, 18.547104, 18.558897, 18.570688, 18.582475,
--    18.594260, 18.606041, 18.617820, 18.629596, 18.641369, 18.653139, 18.664906,
--    18.676671, 18.688432, 18.700191, 18.711947, 18.723700, 18.735450, 18.747197,
--    18.758942, 18.770683, 18.782422, 18.794158, 18.805891, 18.817621, 18.829348,
--    18.841073, 18.852794, 18.864513, 18.876229, 18.887942, 18.899652, 18.911360,
--    18.923064, 18.934766, 18.946465, 18.958161, 18.969855, 18.981545, 18.993233,
--    19.004918, 19.016600, 19.028279, 19.039956, 19.051630, 19.063300, 19.074969,
--    19.086634, 19.098296, 19.109956, 19.121613, 19.133267, 19.144918, 19.156567,
--    19.168213, 19.179855, 19.191496, 19.203133, 19.214768, 19.226399, 19.238028,
--    19.249655, 19.261278, 19.272899, 19.284517, 19.296132, 19.307745, 19.319354,
--    19.330961, 19.342565, 19.354167, 19.365765, 19.377361, 19.388954, 19.400545,
--    19.412132, 19.423717, 19.435299, 19.446879, 19.458456, 19.470029, 19.481601,
--    19.493169, 19.504735, 19.516298, 19.527858, 19.539416, 19.550970, 19.562522,
--    19.574072, 19.585618, 19.597162, 19.608704, 19.620242, 19.631778, 19.643311,
--    19.654841, 19.666369, 19.677894, 19.689416, 19.700936, 19.712452, 19.723966,
--    19.735478, 19.746987, 19.758493, 19.769996, 19.781497, 19.792995, 19.804490,
--    19.815982, 19.827472, 19.838959, 19.850444, 19.861926, 19.873405, 19.884882,
--    19.896355, 19.907826, 19.919295, 19.930761, 19.942224, 19.953684, 19.965142,
--    19.976597, 19.988050, 19.999500, 20.010947, 20.022391, 20.033833, 20.045272,
--    20.056709, 20.068143, 20.079574, 20.091003, 20.102429, 20.113852, 20.125273,
--    20.136691, 20.148106, 20.159519, 20.170929, 20.182336, 20.193741, 20.205144,
--    20.216543, 20.227940, 20.239334, 20.250726, 20.262115, 20.273502, 20.284886,
--    20.296267, 20.307646, 20.319022, 20.330395, 20.341766, 20.353134, 20.364500,
--    20.375863, 20.387223, 20.398581, 20.409936, 20.421288, 20.432638, 20.443986,
--    20.455331, 20.466673, 20.478012, 20.489349, 20.500684, 20.512016, 20.523345,
--    20.534672, 20.545996, 20.557317, 20.568636, 20.579952, 20.591266, 20.602577,
--    20.613886, 20.625192, 20.636496, 20.647796, 20.659095, 20.670391, 20.681684,
--    20.692974, 20.704262, 20.715548, 20.726831, 20.738111, 20.749389, 20.760665,
--    20.771937, 20.783208, 20.794475, 20.805740, 20.817003, 20.828263, 20.839520,
--    20.850775, 20.862028, 20.873278, 20.884525, 20.895770, 20.907012, 20.918252,
--    20.929489, 20.940723, 20.951956, 20.963185, 20.974412, 20.985637, 20.996859,
--    21.008078, 21.019295, 21.030510, 21.041722, 21.052931, 21.064138, 21.075343,
--    21.086544, 21.097744, 21.108941, 21.120135, 21.131327, 21.142516, 21.153703,
--    21.164888, 21.176070, 21.187249, 21.198426, 21.209600, 21.220772, 21.231941,
--    21.243108, 21.254273, 21.265435, 21.276594, 21.287751, 21.298906, 21.310058,
--    21.321207, 21.332354, 21.343499, 21.354641, 21.365780, 21.376918, 21.388052,
--    21.399184, 21.410314, 21.421441, 21.432566, 21.443689, 21.454808, 21.465926,
--    21.477041, 21.488153, 21.499263, 21.510371, 21.521476, 21.532579, 21.543679,
--    21.554776, 21.565872, 21.576965, 21.588055, 21.599143, 21.610228, 21.621312,
--    21.632392, 21.643470, 21.654546, 21.665619, 21.676690, 21.687759, 21.698825,
--    21.709888, 21.720949, 21.732008, 21.743064, 21.754118, 21.765170, 21.776219,
--    21.787265, 21.798309, 21.809351, 21.820390, 21.831427, 21.842462, 21.853494,
--    21.864524, 21.875551, 21.886576, 21.897598, 21.908618, 21.919636, 21.930651,
--    21.941664, 21.952674, 21.963682, 21.974687, 21.985691, 21.996691, 22.007690,
--    22.018686, 22.029679, 22.040671, 22.051659, 22.062646, 22.073630, 22.084612,
--    22.095591, 22.106568, 22.117542, 22.128514, 22.139484, 22.150451, 22.161416,
--    22.172379, 22.183339, 22.194297, 22.205252, 22.216205, 22.227156, 22.238104,
--    22.249050, 22.259994, 22.270935, 22.281874, 22.292811, 22.303745, 22.314676,
--    22.325606, 22.336533, 22.347458, 22.358380, 22.369300, 22.380218, 22.391133,
--    22.402046, 22.412956, 22.423865, 22.434770, 22.445674, 22.456575, 22.467474,
--    22.478371, 22.489265, 22.500157, 22.511046, 22.521933, 22.532818, 22.543700,
--    22.554581, 22.565458, 22.576334, 22.587207, 22.598078, 22.608946, 22.619812,
--    22.630676, 22.641538, 22.652397, 22.663254, 22.674108, 22.684960, 22.695810,
--    22.706658, 22.717503, 22.728346, 22.739187, 22.750025, 22.760861, 22.771695,
--    22.782526, 22.793355, 22.804182, 22.815006, 22.825828, 22.836648, 22.847466,
--    22.858281, 22.869094, 22.879905, 22.890713, 22.901519, 22.912323, 22.923124,
--    22.933923, 22.944720, 22.955515, 22.966307, 22.977097, 22.987885, 22.998670,
--    23.009453, 23.020234, 23.031013, 23.041789, 23.052563, 23.063335, 23.074104,
--    23.084871, 23.095636, 23.106399, 23.117159, 23.127917, 23.138673, 23.149427,
--    23.160178, 23.170927, 23.181674, 23.192418, 23.203160, 23.213900, 23.224638,
--    23.235373, 23.246107, 23.256837, 23.267566, 23.278292, 23.289017, 23.299738,
--    23.310458, 23.321175, 23.331890, 23.342603, 23.353314, 23.364022, 23.374728,
--    23.385432, 23.396134, 23.406833, 23.417530, 23.428225, 23.438918, 23.449608,
--    23.460296, 23.470982, 23.481666, 23.492348, 23.503027, 23.513704, 23.524379,
--    23.535051, 23.545721, 23.556389, 23.567055, 23.577719, 23.588380, 23.599039,
--    23.609696, 23.620351, 23.631004, 23.641654, 23.652302, 23.662948, 23.673591,
--    23.684233, 23.694872, 23.705509, 23.716144, 23.726776, 23.737407, 23.748035,
--    23.758661, 23.769284, 23.779906, 23.790525, 23.801142, 23.811757, 23.822370,
--    23.832980, 23.843589, 23.854195, 23.864799, 23.875400, 23.886000, 23.896597,
--    23.907192, 23.917785, 23.928376, 23.938965, 23.949551, 23.960135, 23.970717,
--    23.981297, 23.991875, 24.002450, 24.013023, 24.023594, 24.034163, 24.044730,
--    24.055294, 24.065857, 24.076417, 24.086975, 24.097531, 24.108084, 24.118636,
--    24.129185, 24.139732, 24.150277, 24.160820, 24.171360, 24.181899, 24.192435,
--    24.202969, 24.213501, 24.224031, 24.234559, 24.245084, 24.255607, 24.266128,
--    24.276647, 24.287164, 24.297679, 24.308191, 24.318702, 24.329210, 24.339716,
--    24.350220, 24.360722, 24.371221, 24.381719, 24.392214, 24.402707, 24.413198,
--    24.423687, 24.434174, 24.444658, 24.455141, 24.465621, 24.476099, 24.486575,
--    24.497049, 24.507521, 24.517991, 24.528458, 24.538923, 24.549387, 24.559848,
--    24.570307, 24.580763, 24.591218, 24.601671, 24.612121, 24.622569, 24.633016,
--    24.643460, 24.653902, 24.664341, 24.674779, 24.685215, 24.695648, 24.706079,
--    24.716509, 24.726936, 24.737361, 24.747784, 24.758204, 24.768623, 24.779040,
--    24.789454, 24.799866, 24.810277, 24.820685, 24.831091, 24.841495, 24.851896,
--    24.862296]
-- }


def horner (x: f64): f64 =
   let (c1,c2,c3,c4,c5) = (0.31938153,-0.356563782,1.781477937,-1.821255978,1.330274429)
   in x * (c1 + x * (c2 + x * (c3 + x * (c4 + x * c5))))

def fabs (x: f64): f64 = if x < 0.0 then -x else x

def cnd0 (d: f64): f64 =
   let k        = 1.0 / (1.0 + 0.2316419 * fabs(d))
   let p        = horner(k)
   let rsqrt2pi = 0.39894228040143267793994605993438 in
   rsqrt2pi * f64.exp(-0.5*d*d) * p

def cnd (d: f64): f64 =
   let c = cnd0(d)
   in if 0.0 < d then 1.0 - c else c

def go (x: (bool,f64,f64,f64)): f64 =
   let (call, price, strike, years) = x
   let r       = 0.08  -- riskfree
   let v       = 0.30  -- volatility
   let v_sqrtT = v * f64.sqrt(years)
   let d1      = (f64.log(price / strike) + (r + 0.5 * v * v) * years) / v_sqrtT
   let d2      = d1 - v_sqrtT
   let cndD1   = cnd(d1)
   let cndD2   = cnd(d2)
   let x_expRT = strike * f64.exp(-r * years) in
   if call then
     price * cndD1 - x_expRT * cndD2
   else
     x_expRT * (1.0 - cndD2) - price * (1.0 - cndD1)

def blackscholes (xs: [](bool,f64,f64,f64)): []f64 =
   map  go xs

def main (years: i64): []f64 =
  let days = years*365
  let a = map (+1) (iota(days))
  let a = map f64.i64 a
  let a = map (\x -> (true, 58.0 + 4.0 * x / f64.i64(days), 65.0, x / 365.0)) a in
  blackscholes(a)
