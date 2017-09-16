enum PokeType: String {
    case normal = "normal"
    case fire = "Fire"
    case fighting = "Fighting"
    case water = "Water"
    case flying = "Flying"
    case grass = "Grass"
    case poison = "Poison"
    case electric = "Electric"
    case ground = "Ground"
    case psychic = "Psychic"
    case rock = "Rock"
    case ice = "Ice"
    case bug = "Bug"
    case dragon = "Dragon"
    case ghost = "Ghost"
    case dark = "Dark"
    case steel = "Steel"
    case fairy = "Fairy"
}

class Pokemon {
    let name: String
    let types: [PokeType]
    
    init(name: String, types: [PokeType]) {
        self.name = name
        self.types = types
    }
}

// gen 0
let bulbasaur = Pokemon(name: "Bulbasaur", types: [.grass, .poison])
let ivysaur = Pokemon(name: "Ivysaur", types: [.grass, .poison])
let venusaur = Pokemon(name: "Venusaur", types: [.grass, .poison])
let charmander = Pokemon(name: "Charmander", types: [.fire])
let charmeleon = Pokemon(name: "Charmeleon", types: [.fire])
let charizard = Pokemon(name: "Charizard", types: [.fire, .flying])
let squirtle = Pokemon(name: "Squirtle", types: [.water])
let wartortle = Pokemon(name: "Wartortle", types: [.water])
let blastoise = Pokemon(name: "Blastoise", types: [.water])
let caterpie = Pokemon(name: "Caterpie", types: [.bug])
let metapod = Pokemon(name: "Metapod", types: [.bug])
let butterfree = Pokemon(name: "Butterfree", types: [.bug, .flying])
let weedle = Pokemon(name: "Weedle", types: [.bug, .poison])
let kakuna = Pokemon(name: "Kakuna", types: [.bug, .poison])
let beedrill = Pokemon(name: "Beedrill", types: [.bug, .poison])
let pidgey = Pokemon(name: "Pidgey", types: [.normal, .flying])
let pidgeotto = Pokemon(name: "Pidgeotto", types: [.normal, .flying])
let pidgeot = Pokemon(name: "Pidgeot", types: [.normal, .flying])
let rattata = Pokemon(name: "Rattata", types: [.normal])
let raticate = Pokemon(name: "Raticate", types: [.normal])
let spearow = Pokemon(name: "Spearow", types: [.normal, .flying])
let fearow = Pokemon(name: "Fearow", types: [.normal, .flying])
let ekans = Pokemon(name: "Ekans", types: [.poison])
let arbok = Pokemon(name: "Arbok", types: [.poison])
let pikachu = Pokemon(name: "Pikachu", types: [.electric])
let raichu = Pokemon(name: "Raichu", types: [.electric])
let sandshrew = Pokemon(name: "Sandshrew", types: [.ground])
let sandslash = Pokemon(name: "Sandslash", types: [.ground])
let nidoranF = Pokemon(name: "Nidoran♀", types: [.poison])
let nidorina = Pokemon(name: "Nidorina", types: [.poison])
let nidoqueen = Pokemon(name: "Nidoqueen", types: [.poison, .ground])
let nidoranM = Pokemon(name: "Nidoran♂", types: [.poison])
let nidorino = Pokemon(name: "Nidorino", types: [.poison])
let nidoking = Pokemon(name: "Nidoking", types: [.poison, .ground])
let clefairy = Pokemon(name: "Clefairy", types: [.fairy])
let clefable = Pokemon(name: "Clefable", types: [.fairy])
let vulpix = Pokemon(name: "Vulpix", types: [.fire])
let ninetales = Pokemon(name: "Ninetales", types: [.fire])
let jigglypuff = Pokemon(name: "Jigglypuff", types: [.normal, .fairy])
let wigglytuff = Pokemon(name: "Wigglytuff", types: [.normal, .fairy])
let zubat = Pokemon(name: "Zubat", types: [.poison, .flying])
let golbat = Pokemon(name: "Golbat", types: [.poison, .flying])
let oddish = Pokemon(name: "Oddish", types: [.grass, .poison])
let gloom = Pokemon(name: "Gloom", types: [.grass, .poison])
let vileplume = Pokemon(name: "Vileplume", types: [.grass, .poison])
let paras = Pokemon(name: "Paras", types: [.bug, .grass])
let parasect = Pokemon(name: "Parasect", types: [.bug, .grass])
let venonat = Pokemon(name: "Venonat", types: [.bug, .poison])
let venomoth = Pokemon(name: "Venomoth", types: [.bug, .poison])
let diglett = Pokemon(name: "Diglett", types: [.ground])
let dugtrio = Pokemon(name: "Dugtrio", types: [.ground])
let meowth = Pokemon(name: "Meowth", types: [.normal])
let persian = Pokemon(name: "Persian", types: [.normal])
let psyduck = Pokemon(name: "Psyduck", types: [.water])
let golduck = Pokemon(name: "Golduck", types: [.water])
let mankey = Pokemon(name: "Mankey", types: [.fighting])
let primeape = Pokemon(name: "Primeape", types: [.fighting])
let growlithe = Pokemon(name: "Growlithe", types: [.fire])
let arcanine = Pokemon(name: "Arcanine", types: [.fire])
let poliwag = Pokemon(name: "Poliwag", types: [.water])
let poliwhirl = Pokemon(name: "Poliwhirl", types: [.water])
let poliwrath = Pokemon(name: "Poliwrath", types: [.water, .fighting])
let abra = Pokemon(name: "Abra", types: [.psychic])
let kadabra = Pokemon(name: "Kadabra", types: [.psychic])
let alakazam = Pokemon(name: "Alakazam", types: [.psychic])
let machop = Pokemon(name: "Machop", types: [.fighting])
let machoke = Pokemon(name: "Machoke", types: [.fighting])
let machamp = Pokemon(name: "Machamp", types: [.fighting])
let bellsprout = Pokemon(name: "Bellsprout", types: [.grass, .poison])
let weepinbell = Pokemon(name: "Weepinbell", types: [.grass, .poison])
let victreebel = Pokemon(name: "Victreebel", types: [.grass, .poison])
let tentacool = Pokemon(name: "Tentacool", types: [.water, .poison])
let tentacruel = Pokemon(name: "Tentacruel", types: [.water, .poison])
let geodude = Pokemon(name: "Geodude", types: [.rock, .ground])
let graveler = Pokemon(name: "Graveler", types: [.rock, .ground])
let golem = Pokemon(name: "Golem", types: [.rock, .ground])
let ponyta = Pokemon(name: "Ponyta", types: [.fire])
let rapidash = Pokemon(name: "Rapidash", types: [.fire])
let slowpoke = Pokemon(name: "Slowpoke", types: [.water, .psychic])
let slowbro = Pokemon(name: "Slowbro", types: [.water, .psychic])
let magnemite = Pokemon(name: "Magnemite", types: [.electric, .steel])
let magneton = Pokemon(name: "Magneton", types: [.electric, .steel])
let farfetchd = Pokemon(name: "Farfetch’d", types: [.normal, .flying])
let doduo = Pokemon(name: "Doduo", types: [.normal, .flying])
let dodrio = Pokemon(name: "Dodrio", types: [.normal, .flying])
let seel = Pokemon(name: "Seel", types: [.water])
let dewgong = Pokemon(name: "Dewgong", types: [.water, .ice])
let grimer = Pokemon(name: "Grimer", types: [.poison])
let muk = Pokemon(name: "Muk", types: [.poison])
let shellder = Pokemon(name: "Shellder", types: [.water])
let cloyster = Pokemon(name: "Cloyster", types: [.water, .ice])
let gastly = Pokemon(name: "Gastly", types: [.ghost, .poison])
let haunter = Pokemon(name: "Haunter", types: [.ghost, .poison])
let gengar = Pokemon(name: "Gengar", types: [.ghost, .poison])
let onix = Pokemon(name: "Onix", types: [.rock, .ground])
let drowzee = Pokemon(name: "Drowzee", types: [.psychic])
let hypno = Pokemon(name: "Hypno", types: [.psychic])
let krabby = Pokemon(name: "Krabby", types: [.water])
let kingler = Pokemon(name: "Kingler", types: [.water])
let voltorb = Pokemon(name: "Voltorb", types: [.electric])
let electrode = Pokemon(name: "Electrode", types: [.electric])
let exeggcute = Pokemon(name: "Exeggcute", types: [.grass, .psychic])
let exeggutor = Pokemon(name: "Exeggutor", types: [.grass, .psychic])
let cubone = Pokemon(name: "Cubone", types: [.ground])
let marowak = Pokemon(name: "Marowak", types: [.ground])
let hitmonlee = Pokemon(name: "Hitmonlee", types: [.fighting])
let hitmonchan = Pokemon(name: "Hitmonchan", types: [.fighting])
let lickitung = Pokemon(name: "Lickitung", types: [.normal])
let koffing = Pokemon(name: "Koffing", types: [.poison])
let weezing = Pokemon(name: "Weezing", types: [.poison])
let rhyhorn = Pokemon(name: "Rhyhorn", types: [.ground, .rock])
let rhydon = Pokemon(name: "Rhydon", types: [.ground, .rock])
let chansey = Pokemon(name: "Chansey", types: [.normal])
let tangela = Pokemon(name: "Tangela", types: [.grass])
let kangaskhan = Pokemon(name: "Kangaskhan", types: [.normal])
let horsea = Pokemon(name: "Horsea", types: [.water])
let seadra = Pokemon(name: "Seadra", types: [.water])
let goldeen = Pokemon(name: "Goldeen", types: [.water])
let seaking = Pokemon(name: "Seaking", types: [.water])
let staryu = Pokemon(name: "Staryu", types: [.water])
let starmie = Pokemon(name: "Starmie", types: [.water, .psychic])
let mr_mime = Pokemon(name: "Mr. Mime", types: [.psychic, .fairy])
let scyther = Pokemon(name: "Scyther", types: [.bug, .flying])
let jynx = Pokemon(name: "Jynx", types: [.ice, .psychic])
let electabuzz = Pokemon(name: "Electabuzz", types: [.electric])
let magmar = Pokemon(name: "Magmar", types: [.fire])
let pinsir = Pokemon(name: "Pinsir", types: [.bug])
let tauros = Pokemon(name: "Tauros", types: [.normal])
let magikarp = Pokemon(name: "Magikarp", types: [.water])
let gyarados = Pokemon(name: "Gyarados", types: [.water, .flying])
let lapras = Pokemon(name: "Lapras", types: [.water, .ice])
let ditto = Pokemon(name: "Ditto", types: [.normal])
let eevee = Pokemon(name: "Eevee", types: [.normal])
let vaporeon = Pokemon(name: "Vaporeon", types: [.water])
let jolteon = Pokemon(name: "Jolteon", types: [.electric])
let flareon = Pokemon(name: "Flareon", types: [.fire])
let porygon = Pokemon(name: "Porygon", types: [.normal])
let omanyte = Pokemon(name: "Omanyte", types: [.rock, .water])
let omastar = Pokemon(name: "Omastar", types: [.rock, .water])
let kabuto = Pokemon(name: "Kabuto", types: [.rock, .water])
let kabutops = Pokemon(name: "Kabutops", types: [.rock, .water])
let aerodactyl = Pokemon(name: "Aerodactyl", types: [.rock, .flying])
let snorlax = Pokemon(name: "Snorlax", types: [.normal])
let articuno = Pokemon(name: "Articuno", types: [.ice, .flying])
let zapdos = Pokemon(name: "Zapdos", types: [.electric, .flying])
let moltres = Pokemon(name: "Moltres", types: [.fire, .flying])
let dratini = Pokemon(name: "Dratini", types: [.dragon])
let dragonair = Pokemon(name: "Dragonair", types: [.dragon])
let dragonite = Pokemon(name: "Dragonite", types: [.dragon, .flying])
let mewtwo = Pokemon(name: "Mewtwo", types: [.psychic])
let mew = Pokemon(name: "Mew", types: [.psychic])


// gen 1
let chikorita = Pokemon(name: "Chikorita", types: [.grass])
let bayleef = Pokemon(name: "Bayleef", types: [.grass])
let meganium = Pokemon(name: "Meganium", types: [.grass])
let cyndaquil = Pokemon(name: "Cyndaquil", types: [.fire])
let quilava = Pokemon(name: "Quilava", types: [.fire])
let typhlosion = Pokemon(name: "Typhlosion", types: [.fire])
let totodile = Pokemon(name: "Totodile", types: [.water])
let croconaw = Pokemon(name: "Croconaw", types: [.water])
let feraligatr = Pokemon(name: "Feraligatr", types: [.water])
let sentret = Pokemon(name: "Sentret", types: [.normal])
let furret = Pokemon(name: "Furret", types: [.normal])
let hoothoot = Pokemon(name: "Hoothoot", types: [.normal, .flying])
let noctowl = Pokemon(name: "Noctowl", types: [.normal, .flying])
let ledyba = Pokemon(name: "Ledyba", types: [.bug, .flying])
let ledian = Pokemon(name: "Ledian", types: [.bug, .flying])
let spinarak = Pokemon(name: "Spinarak", types: [.bug, .poison])
let ariados = Pokemon(name: "Ariados", types: [.bug, .poison])
let crobat = Pokemon(name: "Crobat", types: [.poison, .flying])
let chinchou = Pokemon(name: "Chinchou", types: [.water, .electric])
let lanturn = Pokemon(name: "Lanturn", types: [.water, .electric])
let pichu = Pokemon(name: "Pichu", types: [.electric])
let cleffa = Pokemon(name: "Cleffa", types: [.fairy])
let igglybuff = Pokemon(name: "Igglybuff", types: [.normal, .fairy])
let togepi = Pokemon(name: "Togepi", types: [.fairy])
let togetic = Pokemon(name: "Togetic", types: [.fairy, .flying])
let natu = Pokemon(name: "Natu", types: [.psychic, .flying])
let xatu = Pokemon(name: "Xatu", types: [.psychic, .flying])
let mareep = Pokemon(name: "Mareep", types: [.electric])
let flaaffy = Pokemon(name: "Flaaffy", types: [.electric])
let ampharos = Pokemon(name: "Ampharos", types: [.electric])
let bellossom = Pokemon(name: "Bellossom", types: [.grass])
let marill = Pokemon(name: "Marill", types: [.water, .fairy])
let azumarill = Pokemon(name: "Azumarill", types: [.water, .fairy])
let sudowoodo = Pokemon(name: "Sudowoodo", types: [.rock])
let politoed = Pokemon(name: "Politoed", types: [.water])
let hoppip = Pokemon(name: "Hoppip", types: [.grass, .flying])
let skiploom = Pokemon(name: "Skiploom", types: [.grass, .flying])
let jumpluff = Pokemon(name: "Jumpluff", types: [.grass, .flying])
let aipom = Pokemon(name: "Aipom", types: [.normal])
let sunkern = Pokemon(name: "Sunkern", types: [.grass])
let sunflora = Pokemon(name: "Sunflora", types: [.grass])
let yanma = Pokemon(name: "Yanma", types: [.bug, .flying])
let wooper = Pokemon(name: "Wooper", types: [.water, .ground])
let quagsire = Pokemon(name: "Quagsire", types: [.water, .ground])
let espeon = Pokemon(name: "Espeon", types: [.psychic])
let umbreon = Pokemon(name: "Umbreon", types: [.dark])
let murkrow = Pokemon(name: "Murkrow", types: [.dark, .flying])
let slowking = Pokemon(name: "Slowking", types: [.water, .psychic])
let misdreavus = Pokemon(name: "Misdreavus", types: [.ghost])
let unown = Pokemon(name: "Unown", types: [.psychic])
let wobbuffet = Pokemon(name: "Wobbuffet", types: [.psychic])
let girafarig = Pokemon(name: "Girafarig", types: [.normal, .psychic])
let pineco = Pokemon(name: "Pineco", types: [.bug])
let forretress = Pokemon(name: "Forretress", types: [.bug, .steel])
let dunsparce = Pokemon(name: "Dunsparce", types: [.normal])
let gligar = Pokemon(name: "Gligar", types: [.ground, .flying])
let steelix = Pokemon(name: "Steelix", types: [.steel, .ground])
let snubbull = Pokemon(name: "Snubbull", types: [.fairy])
let granbull = Pokemon(name: "Granbull", types: [.fairy])
let qwilfish = Pokemon(name: "Qwilfish", types: [.water, .poison])
let scizor = Pokemon(name: "Scizor", types: [.bug, .steel])
let shuckle = Pokemon(name: "Shuckle", types: [.bug, .rock])
let heracross = Pokemon(name: "Heracross", types: [.bug, .fighting])
let sneasel = Pokemon(name: "Sneasel", types: [.dark, .ice])
let teddiursa = Pokemon(name: "Teddiursa", types: [.normal])
let ursaring = Pokemon(name: "Ursaring", types: [.normal])
let slugma = Pokemon(name: "Slugma", types: [.fire])
let magcargo = Pokemon(name: "Magcargo", types: [.fire, .rock])
let swinub = Pokemon(name: "Swinub", types: [.ice, .ground])
let piloswine = Pokemon(name: "Piloswine", types: [.ice, .ground])
let corsola = Pokemon(name: "Corsola", types: [.water, .rock])
let remoraid = Pokemon(name: "Remoraid", types: [.water])
let octillery = Pokemon(name: "Octillery", types: [.water])
let delibird = Pokemon(name: "Delibird", types: [.ice, .flying])
let mantine = Pokemon(name: "Mantine", types: [.water, .flying])
let skarmory = Pokemon(name: "Skarmory", types: [.steel, .flying])
let houndour = Pokemon(name: "Houndour", types: [.dark, .fire])
let houndoom = Pokemon(name: "Houndoom", types: [.dark, .fire])
let kingdra = Pokemon(name: "Kingdra", types: [.water, .dragon])
let phanpy = Pokemon(name: "Phanpy", types: [.ground])
let donphan = Pokemon(name: "Donphan", types: [.ground])
let porygon2 = Pokemon(name: "Porygon2", types: [.normal])
let stantler = Pokemon(name: "Stantler", types: [.normal])
let smeargle = Pokemon(name: "Smeargle", types: [.normal])
let tyrogue = Pokemon(name: "Tyrogue", types: [.fighting])
let hitmontop = Pokemon(name: "Hitmontop", types: [.fighting])
let smoochum = Pokemon(name: "Smoochum", types: [.ice, .psychic])
let elekid = Pokemon(name: "Elekid", types: [.electric])
let magby = Pokemon(name: "Magby", types: [.fire])
let miltank = Pokemon(name: "Miltank", types: [.normal])
let blissey = Pokemon(name: "Blissey", types: [.normal])
let raikou = Pokemon(name: "Raikou", types: [.electric])
let entei = Pokemon(name: "Entei", types: [.fire])
let suicune = Pokemon(name: "Suicune", types: [.water])
let larvitar = Pokemon(name: "Larvitar", types: [.rock, .ground])
let pupitar = Pokemon(name: "Pupitar", types: [.rock, .ground])
let tyranitar = Pokemon(name: "Tyranitar", types: [.rock, .dark])
let lugia = Pokemon(name: "Lugia", types: [.psychic, .flying])
let ho_oh = Pokemon(name: "Ho-oh", types: [.fire, .flying])
let celebi = Pokemon(name: "Celebi", types: [.psychic, .grass])


// gen 2
let treecko = Pokemon(name: "Treecko", types: [.grass])
let grovyle = Pokemon(name: "Grovyle", types: [.grass])
let sceptile = Pokemon(name: "Sceptile", types: [.grass])
let torchic = Pokemon(name: "Torchic", types: [.fire])
let combusken = Pokemon(name: "Combusken", types: [.fire, .fighting])
let blaziken = Pokemon(name: "Blaziken", types: [.fire, .fighting])
let mudkip = Pokemon(name: "Mudkip", types: [.water])
let marshtomp = Pokemon(name: "Marshtomp", types: [.water, .ground])
let swampert = Pokemon(name: "Swampert", types: [.water, .ground])
let poochyena = Pokemon(name: "Poochyena", types: [.dark])
let mightyena = Pokemon(name: "Mightyena", types: [.dark])
let zigzagoon = Pokemon(name: "Zigzagoon", types: [.normal])
let linoone = Pokemon(name: "Linoone", types: [.normal])
let wurmple = Pokemon(name: "Wurmple", types: [.bug])
let silcoon = Pokemon(name: "Silcoon", types: [.bug])
let beautifly = Pokemon(name: "Beautifly", types: [.bug, .flying])
let cascoon = Pokemon(name: "Cascoon", types: [.bug])
let dustox = Pokemon(name: "Dustox", types: [.bug, .poison])
let lotad = Pokemon(name: "Lotad", types: [.water, .grass])
let lombre = Pokemon(name: "Lombre", types: [.water, .grass])
let ludicolo = Pokemon(name: "Ludicolo", types: [.water, .grass])
let seedot = Pokemon(name: "Seedot", types: [.grass])
let nuzleaf = Pokemon(name: "Nuzleaf", types: [.grass, .dark])
let shiftry = Pokemon(name: "Shiftry", types: [.grass, .dark])
let taillow = Pokemon(name: "Taillow", types: [.normal, .flying])
let swellow = Pokemon(name: "Swellow", types: [.normal, .flying])
let wingull = Pokemon(name: "Wingull", types: [.water, .flying])
let pelipper = Pokemon(name: "Pelipper", types: [.water, .flying])
let ralts = Pokemon(name: "Ralts", types: [.psychic, .fairy])
let kirlia = Pokemon(name: "Kirlia", types: [.psychic, .fairy])
let gardevoir = Pokemon(name: "Gardevoir", types: [.psychic, .fairy])
let surskit = Pokemon(name: "Surskit", types: [.bug, .water])
let masquerain = Pokemon(name: "Masquerain", types: [.bug, .flying])
let shroomish = Pokemon(name: "Shroomish", types: [.grass])
let breloom = Pokemon(name: "Breloom", types: [.grass, .fighting])
let slakoth = Pokemon(name: "Slakoth", types: [.normal])
let vigoroth = Pokemon(name: "Vigoroth", types: [.normal])
let slaking = Pokemon(name: "Slaking", types: [.normal])
let nincada = Pokemon(name: "Nincada", types: [.bug, .ground])
let ninjask = Pokemon(name: "Ninjask", types: [.bug, .flying])
let shedinja = Pokemon(name: "Shedinja", types: [.bug, .ghost])
let whismur = Pokemon(name: "Whismur", types: [.normal])
let loudred = Pokemon(name: "Loudred", types: [.normal])
let exploud = Pokemon(name: "Exploud", types: [.normal])
let makuhita = Pokemon(name: "Makuhita", types: [.fighting])
let hariyama = Pokemon(name: "Hariyama", types: [.fighting])
let azurill = Pokemon(name: "Azurill", types: [.normal, .fairy])
let nosepass = Pokemon(name: "Nosepass", types: [.rock])
let skitty = Pokemon(name: "Skitty", types: [.normal])
let delcatty = Pokemon(name: "Delcatty", types: [.normal])
let sableye = Pokemon(name: "Sableye", types: [.dark, .ghost])
let mawile = Pokemon(name: "Mawile", types: [.steel, .fairy])
let aron = Pokemon(name: "Aron", types: [.steel, .rock])
let lairon = Pokemon(name: "Lairon", types: [.steel, .rock])
let aggron = Pokemon(name: "Aggron", types: [.steel, .rock])
let meditite = Pokemon(name: "Meditite", types: [.fighting, .psychic])
let medicham = Pokemon(name: "Medicham", types: [.fighting, .psychic])
let electrike = Pokemon(name: "Electrike", types: [.electric])
let manectric = Pokemon(name: "Manectric", types: [.electric])
let plusle = Pokemon(name: "Plusle", types: [.electric])
let minun = Pokemon(name: "Minun", types: [.electric])
let volbeat = Pokemon(name: "Volbeat", types: [.bug])
let illumise = Pokemon(name: "Illumise", types: [.bug])
let roselia = Pokemon(name: "Roselia", types: [.grass, .poison])
let gulpin = Pokemon(name: "Gulpin", types: [.poison])
let swalot = Pokemon(name: "Swalot", types: [.poison])
let carvanha = Pokemon(name: "Carvanha", types: [.water, .dark])
let sharpedo = Pokemon(name: "Sharpedo", types: [.water, .dark])
let wailmer = Pokemon(name: "Wailmer", types: [.water])
let wailord = Pokemon(name: "Wailord", types: [.water])
let numel = Pokemon(name: "Numel", types: [.fire, .ground])
let camerupt = Pokemon(name: "Camerupt", types: [.fire, .ground])
let torkoal = Pokemon(name: "Torkoal", types: [.fire])
let spoink = Pokemon(name: "Spoink", types: [.psychic])
let grumpig = Pokemon(name: "Grumpig", types: [.psychic])
let spinda = Pokemon(name: "Spinda", types: [.normal])
let trapinch = Pokemon(name: "Trapinch", types: [.ground])
let vibrava = Pokemon(name: "Vibrava", types: [.ground, .dragon])
let flygon = Pokemon(name: "Flygon", types: [.ground, .dragon])
let cacnea = Pokemon(name: "Cacnea", types: [.grass])
let cacturne = Pokemon(name: "Cacturne", types: [.grass, .dark])
let swablu = Pokemon(name: "Swablu", types: [.normal, .flying])
let altaria = Pokemon(name: "Altaria", types: [.dragon, .flying])
let zangoose = Pokemon(name: "Zangoose", types: [.normal])
let seviper = Pokemon(name: "Seviper", types: [.poison])
let lunatone = Pokemon(name: "Lunatone", types: [.rock, .psychic])
let solrock = Pokemon(name: "Solrock", types: [.rock, .psychic])
let barboach = Pokemon(name: "Barboach", types: [.water, .ground])
let whiscash = Pokemon(name: "Whiscash", types: [.water, .ground])
let corphish = Pokemon(name: "Corphish", types: [.water])
let crawdaunt = Pokemon(name: "Crawdaunt", types: [.water, .dark])
let baltoy = Pokemon(name: "Baltoy", types: [.ground, .psychic])
let claydol = Pokemon(name: "Claydol", types: [.ground, .psychic])
let lileep = Pokemon(name: "Lileep", types: [.rock, .grass])
let cradily = Pokemon(name: "Cradily", types: [.rock, .grass])
let anorith = Pokemon(name: "Anorith", types: [.rock, .bug])
let armaldo = Pokemon(name: "Armaldo", types: [.rock, .bug])
let feebas = Pokemon(name: "Feebas", types: [.water])
let milotic = Pokemon(name: "Milotic", types: [.water])
let castform = Pokemon(name: "Castform", types: [.normal])
let kecleon = Pokemon(name: "Kecleon", types: [.normal])
let shuppet = Pokemon(name: "Shuppet", types: [.ghost])
let banette = Pokemon(name: "Banette", types: [.ghost])
let duskull = Pokemon(name: "Duskull", types: [.ghost])
let dusclops = Pokemon(name: "Dusclops", types: [.ghost])
let tropius = Pokemon(name: "Tropius", types: [.grass, .flying])
let chimecho = Pokemon(name: "Chimecho", types: [.psychic])
let absol = Pokemon(name: "Absol", types: [.dark])
let wynaut = Pokemon(name: "Wynaut", types: [.psychic])
let snorunt = Pokemon(name: "Snorunt", types: [.ice])
let glalie = Pokemon(name: "Glalie", types: [.ice])
let spheal = Pokemon(name: "Spheal", types: [.ice, .water])
let sealeo = Pokemon(name: "Sealeo", types: [.ice, .water])
let walrein = Pokemon(name: "Walrein", types: [.ice, .water])
let clamperl = Pokemon(name: "Clamperl", types: [.water])
let huntail = Pokemon(name: "Huntail", types: [.water])
let gorebyss = Pokemon(name: "Gorebyss", types: [.water])
let relicanth = Pokemon(name: "Relicanth", types: [.water, .rock])
let luvdisc = Pokemon(name: "Luvdisc", types: [.water])
let bagon = Pokemon(name: "Bagon", types: [.dragon])
let shelgon = Pokemon(name: "Shelgon", types: [.dragon])
let salamence = Pokemon(name: "Salamence", types: [.dragon, .flying])
let beldum = Pokemon(name: "Beldum", types: [.steel, .psychic])
let metang = Pokemon(name: "Metang", types: [.steel, .psychic])
let metagross = Pokemon(name: "Metagross", types: [.steel, .psychic])
let regirock = Pokemon(name: "Regirock", types: [.rock])
let regice = Pokemon(name: "Regice", types: [.ice])
let registeel = Pokemon(name: "Registeel", types: [.steel])
let latias = Pokemon(name: "Latias", types: [.dragon, .psychic])
let latios = Pokemon(name: "Latios", types: [.dragon, .psychic])
let kyogre = Pokemon(name: "Kyogre", types: [.water])
let groudon = Pokemon(name: "Groudon", types: [.ground])
let rayquaza = Pokemon(name: "Rayquaza", types: [.dragon, .flying])
let jirachi = Pokemon(name: "Jirachi", types: [.steel, .psychic])
let deoxys = Pokemon(name: "Deoxys", types: [.psychic])


// gen 3
let turtwig = Pokemon(name: "Turtwig", types: [.grass])
let grotle = Pokemon(name: "Grotle", types: [.grass])
let torterra = Pokemon(name: "Torterra", types: [.grass, .ground])
let chimchar = Pokemon(name: "Chimchar", types: [.fire])
let monferno = Pokemon(name: "Monferno", types: [.fire, .fighting])
let infernape = Pokemon(name: "Infernape", types: [.fire, .fighting])
let piplup = Pokemon(name: "Piplup", types: [.water])
let prinplup = Pokemon(name: "Prinplup", types: [.water])
let empoleon = Pokemon(name: "Empoleon", types: [.water, .steel])
let starly = Pokemon(name: "Starly", types: [.normal, .flying])
let staravia = Pokemon(name: "Staravia", types: [.normal, .flying])
let staraptor = Pokemon(name: "Staraptor", types: [.normal, .flying])
let bidoof = Pokemon(name: "Bidoof", types: [.normal])
let bibarel = Pokemon(name: "Bibarel", types: [.normal, .water])
let kricketot = Pokemon(name: "Kricketot", types: [.bug])
let kricketune = Pokemon(name: "Kricketune", types: [.bug])
let shinx = Pokemon(name: "Shinx", types: [.electric])
let luxio = Pokemon(name: "Luxio", types: [.electric])
let luxray = Pokemon(name: "Luxray", types: [.electric])
let budew = Pokemon(name: "Budew", types: [.grass, .poison])
let roserade = Pokemon(name: "Roserade", types: [.grass, .poison])
let cranidos = Pokemon(name: "Cranidos", types: [.rock])
let rampardos = Pokemon(name: "Rampardos", types: [.rock])
let shieldon = Pokemon(name: "Shieldon", types: [.rock, .steel])
let bastiodon = Pokemon(name: "Bastiodon", types: [.rock, .steel])
let burmy = Pokemon(name: "Burmy", types: [.bug])
let wormadam = Pokemon(name: "Wormadam", types: [.bug, .grass])
let mothim = Pokemon(name: "Mothim", types: [.bug, .flying])
let combee = Pokemon(name: "Combee", types: [.bug, .flying])
let vespiquen = Pokemon(name: "Vespiquen", types: [.bug, .flying])
let pachirisu = Pokemon(name: "Pachirisu", types: [.electric])
let buizel = Pokemon(name: "Buizel", types: [.water])
let floatzel = Pokemon(name: "Floatzel", types: [.water])
let cherubi = Pokemon(name: "Cherubi", types: [.grass])
let cherrim = Pokemon(name: "Cherrim", types: [.grass])
let shellos = Pokemon(name: "Shellos", types: [.water])
let gastrodon = Pokemon(name: "Gastrodon", types: [.water, .ground])
let ambipom = Pokemon(name: "Ambipom", types: [.normal])
let drifloon = Pokemon(name: "Drifloon", types: [.ghost, .flying])
let drifblim = Pokemon(name: "Drifblim", types: [.ghost, .flying])
let buneary = Pokemon(name: "Buneary", types: [.normal])
let lopunny = Pokemon(name: "Lopunny", types: [.normal])
let mismagius = Pokemon(name: "Mismagius", types: [.ghost])
let honchkrow = Pokemon(name: "Honchkrow", types: [.dark, .flying])
let glameow = Pokemon(name: "Glameow", types: [.normal])
let purugly = Pokemon(name: "Purugly", types: [.normal])
let chingling = Pokemon(name: "Chingling", types: [.psychic])
let stunky = Pokemon(name: "Stunky", types: [.poison, .dark])
let skuntank = Pokemon(name: "Skuntank", types: [.poison, .dark])
let bronzor = Pokemon(name: "Bronzor", types: [.steel, .psychic])
let bronzong = Pokemon(name: "Bronzong", types: [.steel, .psychic])
let bonsly = Pokemon(name: "Bonsly", types: [.rock])
let mime_jr = Pokemon(name: "Mime Jr.", types: [.psychic, .fairy])
let happiny = Pokemon(name: "Happiny", types: [.normal])
let chatot = Pokemon(name: "Chatot", types: [.normal, .flying])
let spiritomb = Pokemon(name: "Spiritomb", types: [.ghost, .dark])
let gible = Pokemon(name: "Gible", types: [.dragon, .ground])
let gabite = Pokemon(name: "Gabite", types: [.dragon, .ground])
let garchomp = Pokemon(name: "Garchomp", types: [.dragon, .ground])
let munchlax = Pokemon(name: "Munchlax", types: [.normal])
let riolu = Pokemon(name: "Riolu", types: [.fighting])
let lucario = Pokemon(name: "Lucario", types: [.fighting, .steel])
let hippopotas = Pokemon(name: "Hippopotas", types: [.ground])
let hippowdon = Pokemon(name: "Hippowdon", types: [.ground])
let skorupi = Pokemon(name: "Skorupi", types: [.poison, .bug])
let drapion = Pokemon(name: "Drapion", types: [.poison, .dark])
let croagunk = Pokemon(name: "Croagunk", types: [.poison, .fighting])
let toxicroak = Pokemon(name: "Toxicroak", types: [.poison, .fighting])
let carnivine = Pokemon(name: "Carnivine", types: [.grass])
let finneon = Pokemon(name: "Finneon", types: [.water])
let lumineon = Pokemon(name: "Lumineon", types: [.water])
let mantyke = Pokemon(name: "Mantyke", types: [.water, .flying])
let snover = Pokemon(name: "Snover", types: [.grass, .ice])
let abomasnow = Pokemon(name: "Abomasnow", types: [.grass, .ice])
let weavile = Pokemon(name: "Weavile", types: [.dark, .ice])
let magnezone = Pokemon(name: "Magnezone", types: [.electric, .steel])
let lickilicky = Pokemon(name: "Lickilicky", types: [.normal])
let rhyperior = Pokemon(name: "Rhyperior", types: [.ground, .rock])
let tangrowth = Pokemon(name: "Tangrowth", types: [.grass])
let electivire = Pokemon(name: "Electivire", types: [.electric])
let magmortar = Pokemon(name: "Magmortar", types: [.fire])
let togekiss = Pokemon(name: "Togekiss", types: [.fairy, .flying])
let yanmega = Pokemon(name: "Yanmega", types: [.bug, .flying])
let leafeon = Pokemon(name: "Leafeon", types: [.grass])
let glaceon = Pokemon(name: "Glaceon", types: [.ice])
let gliscor = Pokemon(name: "Gliscor", types: [.ground, .flying])
let mamoswine = Pokemon(name: "Mamoswine", types: [.ice, .ground])
let porygon_z = Pokemon(name: "Porygon-Z", types: [.normal])
let gallade = Pokemon(name: "Gallade", types: [.psychic, .fighting])
let probopass = Pokemon(name: "Probopass", types: [.rock, .steel])
let dusknoir = Pokemon(name: "Dusknoir", types: [.ghost])
let froslass = Pokemon(name: "Froslass", types: [.ice, .ghost])
let rotom = Pokemon(name: "Rotom", types: [.electric, .ghost])
let uxie = Pokemon(name: "Uxie", types: [.psychic])
let mesprit = Pokemon(name: "Mesprit", types: [.psychic])
let azelf = Pokemon(name: "Azelf", types: [.psychic])
let dialga = Pokemon(name: "Dialga", types: [.steel, .dragon])
let palkia = Pokemon(name: "Palkia", types: [.water, .dragon])
let heatran = Pokemon(name: "Heatran", types: [.fire, .steel])
let regigigas = Pokemon(name: "Regigigas", types: [.normal])
let giratina = Pokemon(name: "Giratina", types: [.ghost, .dragon])
let cresselia = Pokemon(name: "Cresselia", types: [.psychic])
let phione = Pokemon(name: "Phione", types: [.water])
let manaphy = Pokemon(name: "Manaphy", types: [.water])
let darkrai = Pokemon(name: "Darkrai", types: [.dark])
let shaymin = Pokemon(name: "Shaymin", types: [.grass])
let arceus = Pokemon(name: "Arceus", types: [.normal])


// gen 4
let victini = Pokemon(name: "Victini", types: [.psychic, .fire])
let snivy = Pokemon(name: "Snivy", types: [.grass])
let servine = Pokemon(name: "Servine", types: [.grass])
let serperior = Pokemon(name: "Serperior", types: [.grass])
let tepig = Pokemon(name: "Tepig", types: [.fire])
let pignite = Pokemon(name: "Pignite", types: [.fire, .fighting])
let emboar = Pokemon(name: "Emboar", types: [.fire, .fighting])
let oshawott = Pokemon(name: "Oshawott", types: [.water])
let dewott = Pokemon(name: "Dewott", types: [.water])
let samurott = Pokemon(name: "Samurott", types: [.water])
let patrat = Pokemon(name: "Patrat", types: [.normal])
let watchog = Pokemon(name: "Watchog", types: [.normal])
let lillipup = Pokemon(name: "Lillipup", types: [.normal])
let herdier = Pokemon(name: "Herdier", types: [.normal])
let stoutland = Pokemon(name: "Stoutland", types: [.normal])
let purrloin = Pokemon(name: "Purrloin", types: [.dark])
let liepard = Pokemon(name: "Liepard", types: [.dark])
let pansage = Pokemon(name: "Pansage", types: [.grass])
let simisage = Pokemon(name: "Simisage", types: [.grass])
let pansear = Pokemon(name: "Pansear", types: [.fire])
let simisear = Pokemon(name: "Simisear", types: [.fire])
let panpour = Pokemon(name: "Panpour", types: [.water])
let simipour = Pokemon(name: "Simipour", types: [.water])
let munna = Pokemon(name: "Munna", types: [.psychic])
let musharna = Pokemon(name: "Musharna", types: [.psychic])
let pidove = Pokemon(name: "Pidove", types: [.normal, .flying])
let tranquill = Pokemon(name: "Tranquill", types: [.normal, .flying])
let unfezant = Pokemon(name: "Unfezant", types: [.normal, .flying])
let blitzle = Pokemon(name: "Blitzle", types: [.electric])
let zebstrika = Pokemon(name: "Zebstrika", types: [.electric])
let roggenrola = Pokemon(name: "Roggenrola", types: [.rock])
let boldore = Pokemon(name: "Boldore", types: [.rock])
let gigalith = Pokemon(name: "Gigalith", types: [.rock])
let woobat = Pokemon(name: "Woobat", types: [.psychic, .flying])
let swoobat = Pokemon(name: "Swoobat", types: [.psychic, .flying])
let drilbur = Pokemon(name: "Drilbur", types: [.ground])
let excadrill = Pokemon(name: "Excadrill", types: [.ground, .steel])
let audino = Pokemon(name: "Audino", types: [.normal])
let timburr = Pokemon(name: "Timburr", types: [.fighting])
let gurdurr = Pokemon(name: "Gurdurr", types: [.fighting])
let conkeldurr = Pokemon(name: "Conkeldurr", types: [.fighting])
let tympole = Pokemon(name: "Tympole", types: [.water])
let palpitoad = Pokemon(name: "Palpitoad", types: [.water, .ground])
let seismitoad = Pokemon(name: "Seismitoad", types: [.water, .ground])
let throh = Pokemon(name: "Throh", types: [.fighting])
let sawk = Pokemon(name: "Sawk", types: [.fighting])
let sewaddle = Pokemon(name: "Sewaddle", types: [.bug, .grass])
let swadloon = Pokemon(name: "Swadloon", types: [.bug, .grass])
let leavanny = Pokemon(name: "Leavanny", types: [.bug, .grass])
let venipede = Pokemon(name: "Venipede", types: [.bug, .poison])
let whirlipede = Pokemon(name: "Whirlipede", types: [.bug, .poison])
let scolipede = Pokemon(name: "Scolipede", types: [.bug, .poison])
let cottonee = Pokemon(name: "Cottonee", types: [.grass, .fairy])
let whimsicott = Pokemon(name: "Whimsicott", types: [.grass, .fairy])
let petilil = Pokemon(name: "Petilil", types: [.grass])
let lilligant = Pokemon(name: "Lilligant", types: [.grass])
let basculin = Pokemon(name: "Basculin", types: [.water])
let sandile = Pokemon(name: "Sandile", types: [.ground, .dark])
let krokorok = Pokemon(name: "Krokorok", types: [.ground, .dark])
let krookodile = Pokemon(name: "Krookodile", types: [.ground, .dark])
let darumaka = Pokemon(name: "Darumaka", types: [.fire])
let darmanitan = Pokemon(name: "Darmanitan", types: [.fire])
let maractus = Pokemon(name: "Maractus", types: [.grass])
let dwebble = Pokemon(name: "Dwebble", types: [.bug, .rock])
let crustle = Pokemon(name: "Crustle", types: [.bug, .rock])
let scraggy = Pokemon(name: "Scraggy", types: [.dark, .fighting])
let scrafty = Pokemon(name: "Scrafty", types: [.dark, .fighting])
let sigilyph = Pokemon(name: "Sigilyph", types: [.psychic, .flying])
let yamask = Pokemon(name: "Yamask", types: [.ghost])
let cofagrigus = Pokemon(name: "Cofagrigus", types: [.ghost])
let tirtouga = Pokemon(name: "Tirtouga", types: [.water, .rock])
let carracosta = Pokemon(name: "Carracosta", types: [.water, .rock])
let archen = Pokemon(name: "Archen", types: [.rock, .flying])
let archeops = Pokemon(name: "Archeops", types: [.rock, .flying])
let trubbish = Pokemon(name: "Trubbish", types: [.poison])
let garbodor = Pokemon(name: "Garbodor", types: [.poison])
let zorua = Pokemon(name: "Zorua", types: [.dark])
let zoroark = Pokemon(name: "Zoroark", types: [.dark])
let minccino = Pokemon(name: "Minccino", types: [.normal])
let cinccino = Pokemon(name: "Cinccino", types: [.normal])
let gothita = Pokemon(name: "Gothita", types: [.psychic])
let gothorita = Pokemon(name: "Gothorita", types: [.psychic])
let gothitelle = Pokemon(name: "Gothitelle", types: [.psychic])
let solosis = Pokemon(name: "Solosis", types: [.psychic])
let duosion = Pokemon(name: "Duosion", types: [.psychic])
let reuniclus = Pokemon(name: "Reuniclus", types: [.psychic])
let ducklett = Pokemon(name: "Ducklett", types: [.water, .flying])
let swanna = Pokemon(name: "Swanna", types: [.water, .flying])
let vanillite = Pokemon(name: "Vanillite", types: [.ice])
let vanillish = Pokemon(name: "Vanillish", types: [.ice])
let vanilluxe = Pokemon(name: "Vanilluxe", types: [.ice])
let deerling = Pokemon(name: "Deerling", types: [.normal, .grass])
let sawsbuck = Pokemon(name: "Sawsbuck", types: [.normal, .grass])
let emolga = Pokemon(name: "Emolga", types: [.electric, .flying])
let karrablast = Pokemon(name: "Karrablast", types: [.bug])
let escavalier = Pokemon(name: "Escavalier", types: [.bug, .steel])
let foongus = Pokemon(name: "Foongus", types: [.grass, .poison])
let amoonguss = Pokemon(name: "Amoonguss", types: [.grass, .poison])
let frillish = Pokemon(name: "Frillish", types: [.water, .ghost])
let jellicent = Pokemon(name: "Jellicent", types: [.water, .ghost])
let alomomola = Pokemon(name: "Alomomola", types: [.water])
let joltik = Pokemon(name: "Joltik", types: [.bug, .electric])
let galvantula = Pokemon(name: "Galvantula", types: [.bug, .electric])
let ferroseed = Pokemon(name: "Ferroseed", types: [.grass, .steel])
let ferrothorn = Pokemon(name: "Ferrothorn", types: [.grass, .steel])
let klink = Pokemon(name: "Klink", types: [.steel])
let klang = Pokemon(name: "Klang", types: [.steel])
let klinklang = Pokemon(name: "Klinklang", types: [.steel])
let tynamo = Pokemon(name: "Tynamo", types: [.electric])
let eelektrik = Pokemon(name: "Eelektrik", types: [.electric])
let eelektross = Pokemon(name: "Eelektross", types: [.electric])
let elgyem = Pokemon(name: "Elgyem", types: [.psychic])
let beheeyem = Pokemon(name: "Beheeyem", types: [.psychic])
let litwick = Pokemon(name: "Litwick", types: [.ghost, .fire])
let lampent = Pokemon(name: "Lampent", types: [.ghost, .fire])
let chandelure = Pokemon(name: "Chandelure", types: [.ghost, .fire])
let axew = Pokemon(name: "Axew", types: [.dragon])
let fraxure = Pokemon(name: "Fraxure", types: [.dragon])
let haxorus = Pokemon(name: "Haxorus", types: [.dragon])
let cubchoo = Pokemon(name: "Cubchoo", types: [.ice])
let beartic = Pokemon(name: "Beartic", types: [.ice])
let cryogonal = Pokemon(name: "Cryogonal", types: [.ice])
let shelmet = Pokemon(name: "Shelmet", types: [.bug])
let accelgor = Pokemon(name: "Accelgor", types: [.bug])
let stunfisk = Pokemon(name: "Stunfisk", types: [.electric, .ground])
let mienfoo = Pokemon(name: "Mienfoo", types: [.fighting])
let mienshao = Pokemon(name: "Mienshao", types: [.fighting])
let druddigon = Pokemon(name: "Druddigon", types: [.dragon])
let golett = Pokemon(name: "Golett", types: [.ground, .ghost])
let golurk = Pokemon(name: "Golurk", types: [.ground, .ghost])
let pawniard = Pokemon(name: "Pawniard", types: [.dark, .steel])
let bisharp = Pokemon(name: "Bisharp", types: [.dark, .steel])
let bouffalant = Pokemon(name: "Bouffalant", types: [.normal])
let rufflet = Pokemon(name: "Rufflet", types: [.normal, .flying])
let braviary = Pokemon(name: "Braviary", types: [.normal, .flying])
let vullaby = Pokemon(name: "Vullaby", types: [.dark, .flying])
let mandibuzz = Pokemon(name: "Mandibuzz", types: [.dark, .flying])
let heatmor = Pokemon(name: "Heatmor", types: [.fire])
let durant = Pokemon(name: "Durant", types: [.bug, .steel])
let deino = Pokemon(name: "Deino", types: [.dark, .dragon])
let zweilous = Pokemon(name: "Zweilous", types: [.dark, .dragon])
let hydreigon = Pokemon(name: "Hydreigon", types: [.dark, .dragon])
let larvesta = Pokemon(name: "Larvesta", types: [.bug, .fire])
let volcarona = Pokemon(name: "Volcarona", types: [.bug, .fire])
let cobalion = Pokemon(name: "Cobalion", types: [.steel, .fighting])
let terrakion = Pokemon(name: "Terrakion", types: [.rock, .fighting])
let virizion = Pokemon(name: "Virizion", types: [.grass, .fighting])
let tornadus = Pokemon(name: "Tornadus", types: [.flying])
let thundurus = Pokemon(name: "Thundurus", types: [.electric, .flying])
let reshiram = Pokemon(name: "Reshiram", types: [.dragon, .fire])
let zekrom = Pokemon(name: "Zekrom", types: [.dragon, .electric])
let landorus = Pokemon(name: "Landorus", types: [.ground, .flying])
let kyurem = Pokemon(name: "Kyurem", types: [.dragon, .ice])
let keldeo = Pokemon(name: "Keldeo", types: [.water, .fighting])
let meloetta = Pokemon(name: "Meloetta", types: [.normal, .psychic])
let genesect = Pokemon(name: "Genesect", types: [.bug, .steel])


// gen 5
let chespin = Pokemon(name: "Chespin", types: [.grass])
let quilladin = Pokemon(name: "Quilladin", types: [.grass])
let chesnaught = Pokemon(name: "Chesnaught", types: [.grass, .fighting])
let fennekin = Pokemon(name: "Fennekin", types: [.fire])
let braixen = Pokemon(name: "Braixen", types: [.fire])
let delphox = Pokemon(name: "Delphox", types: [.fire, .psychic])
let froakie = Pokemon(name: "Froakie", types: [.water])
let frogadier = Pokemon(name: "Frogadier", types: [.water])
let greninja = Pokemon(name: "Greninja", types: [.water, .dark])
let bunnelby = Pokemon(name: "Bunnelby", types: [.normal])
let diggersby = Pokemon(name: "Diggersby", types: [.normal, .ground])
let fletchling = Pokemon(name: "Fletchling", types: [.normal, .flying])
let fletchinder = Pokemon(name: "Fletchinder", types: [.fire, .flying])
let talonflame = Pokemon(name: "Talonflame", types: [.fire, .flying])
let scatterbug = Pokemon(name: "Scatterbug", types: [.bug])
let spewpa = Pokemon(name: "Spewpa", types: [.bug])
let vivillon = Pokemon(name: "Vivillon", types: [.bug, .flying])
let litleo = Pokemon(name: "Litleo", types: [.fire, .normal])
let pyroar = Pokemon(name: "Pyroar", types: [.fire, .normal])
let flabébé = Pokemon(name: "Flabébé", types: [.fairy])
let floette = Pokemon(name: "Floette", types: [.fairy])
let florges = Pokemon(name: "Florges", types: [.fairy])
let skiddo = Pokemon(name: "Skiddo", types: [.grass])
let gogoat = Pokemon(name: "Gogoat", types: [.grass])
let pancham = Pokemon(name: "Pancham", types: [.fighting])
let pangoro = Pokemon(name: "Pangoro", types: [.fighting, .dark])
let furfrou = Pokemon(name: "Furfrou", types: [.normal])
let espurr = Pokemon(name: "Espurr", types: [.psychic])
let meowstic = Pokemon(name: "Meowstic", types: [.psychic])
let honedge = Pokemon(name: "Honedge", types: [.steel, .ghost])
let doublade = Pokemon(name: "Doublade", types: [.steel, .ghost])
let aegislash = Pokemon(name: "Aegislash", types: [.steel, .ghost])
let spritzee = Pokemon(name: "Spritzee", types: [.fairy])
let aromatisse = Pokemon(name: "Aromatisse", types: [.fairy])
let swirlix = Pokemon(name: "Swirlix", types: [.fairy])
let slurpuff = Pokemon(name: "Slurpuff", types: [.fairy])
let inkay = Pokemon(name: "Inkay", types: [.dark, .psychic])
let malamar = Pokemon(name: "Malamar", types: [.dark, .psychic])
let binacle = Pokemon(name: "Binacle", types: [.rock, .water])
let barbaracle = Pokemon(name: "Barbaracle", types: [.rock, .water])
let skrelp = Pokemon(name: "Skrelp", types: [.poison, .water])
let dragalge = Pokemon(name: "Dragalge", types: [.poison, .dragon])
let clauncher = Pokemon(name: "Clauncher", types: [.water])
let clawitzer = Pokemon(name: "Clawitzer", types: [.water])
let helioptile = Pokemon(name: "Helioptile", types: [.electric, .normal])
let heliolisk = Pokemon(name: "Heliolisk", types: [.electric, .normal])
let tyrunt = Pokemon(name: "Tyrunt", types: [.rock, .dragon])
let tyrantrum = Pokemon(name: "Tyrantrum", types: [.rock, .dragon])
let amaura = Pokemon(name: "Amaura", types: [.rock, .ice])
let aurorus = Pokemon(name: "Aurorus", types: [.rock, .ice])
let sylveon = Pokemon(name: "Sylveon", types: [.fairy])
let hawlucha = Pokemon(name: "Hawlucha", types: [.fighting, .flying])
let dedenne = Pokemon(name: "Dedenne", types: [.electric, .fairy])
let carbink = Pokemon(name: "Carbink", types: [.rock, .fairy])
let goomy = Pokemon(name: "Goomy", types: [.dragon])
let sliggoo = Pokemon(name: "Sliggoo", types: [.dragon])
let goodra = Pokemon(name: "Goodra", types: [.dragon])
let klefki = Pokemon(name: "Klefki", types: [.steel, .fairy])
let phantump = Pokemon(name: "Phantump", types: [.ghost, .grass])
let trevenant = Pokemon(name: "Trevenant", types: [.ghost, .grass])
let pumpkaboo = Pokemon(name: "Pumpkaboo", types: [.ghost, .grass])
let gourgeist = Pokemon(name: "Gourgeist", types: [.ghost, .grass])
let bergmite = Pokemon(name: "Bergmite", types: [.ice])
let avalugg = Pokemon(name: "Avalugg", types: [.ice])
let noibat = Pokemon(name: "Noibat", types: [.flying, .dragon])
let noivern = Pokemon(name: "Noivern", types: [.flying, .dragon])
let xerneas = Pokemon(name: "Xerneas", types: [.fairy])
let yveltal = Pokemon(name: "Yveltal", types: [.dark, .flying])
let zygarde = Pokemon(name: "Zygarde", types: [.dragon, .ground])
let diancie = Pokemon(name: "Diancie", types: [.rock, .fairy])
let hoopa = Pokemon(name: "Hoopa", types: [.psychic, .ghost])
let volcanion = Pokemon(name: "Volcanion", types: [.fire, .water])


// gen 6
let rowlet = Pokemon(name: "Rowlet", types: [.grass, .flying])
let dartrix = Pokemon(name: "Dartrix", types: [.grass, .flying])
let decidueye = Pokemon(name: "Decidueye", types: [.grass, .ghost])
let litten = Pokemon(name: "Litten", types: [.fire])
let torracat = Pokemon(name: "Torracat", types: [.fire])
let incineroar = Pokemon(name: "Incineroar", types: [.fire, .dark])
let popplio = Pokemon(name: "Popplio", types: [.water])
let brionne = Pokemon(name: "Brionne", types: [.water])
let primarina = Pokemon(name: "Primarina", types: [.water, .fairy])
let pikipek = Pokemon(name: "Pikipek", types: [.normal, .flying])
let trumbeak = Pokemon(name: "Trumbeak", types: [.normal, .flying])
let toucannon = Pokemon(name: "Toucannon", types: [.normal, .flying])
let yungoos = Pokemon(name: "Yungoos", types: [.normal])
let gumshoos = Pokemon(name: "Gumshoos", types: [.normal])
let grubbin = Pokemon(name: "Grubbin", types: [.bug])
let charjabug = Pokemon(name: "Charjabug", types: [.bug, .electric])
let vikavolt = Pokemon(name: "Vikavolt", types: [.bug, .electric])
let crabrawler = Pokemon(name: "Crabrawler", types: [.fighting])
let crabominable = Pokemon(name: "Crabominable", types: [.fighting, .ice])
let oricorio = Pokemon(name: "Oricorio", types: [.fire, .flying])
let cutiefly = Pokemon(name: "Cutiefly", types: [.bug, .fairy])
let ribombee = Pokemon(name: "Ribombee", types: [.bug, .fairy])
let rockruff = Pokemon(name: "Rockruff", types: [.rock])
let lycanroc = Pokemon(name: "Lycanroc", types: [.rock])
let wishiwashi = Pokemon(name: "Wishiwashi", types: [.water])
let mareanie = Pokemon(name: "Mareanie", types: [.poison, .water])
let toxapex = Pokemon(name: "Toxapex", types: [.poison, .water])
let mudbray = Pokemon(name: "Mudbray", types: [.ground])
let mudsdale = Pokemon(name: "Mudsdale", types: [.ground])
let dewpider = Pokemon(name: "Dewpider", types: [.water, .bug])
let araquanid = Pokemon(name: "Araquanid", types: [.water, .bug])
let fomantis = Pokemon(name: "Fomantis", types: [.grass])
let lurantis = Pokemon(name: "Lurantis", types: [.grass])
let morelull = Pokemon(name: "Morelull", types: [.grass, .fairy])
let shiinotic = Pokemon(name: "Shiinotic", types: [.grass, .fairy])
let salandit = Pokemon(name: "Salandit", types: [.poison, .fire])
let salazzle = Pokemon(name: "Salazzle", types: [.poison, .fire])
let stufful = Pokemon(name: "Stufful", types: [.normal, .fighting])
let bewear = Pokemon(name: "Bewear", types: [.normal, .fighting])
let bounsweet = Pokemon(name: "Bounsweet", types: [.grass])
let steenee = Pokemon(name: "Steenee", types: [.grass])
let tsareena = Pokemon(name: "Tsareena", types: [.grass])
let comfey = Pokemon(name: "Comfey", types: [.fairy])
let oranguru = Pokemon(name: "Oranguru", types: [.normal, .psychic])
let passimian = Pokemon(name: "Passimian", types: [.fighting])
let wimpod = Pokemon(name: "Wimpod", types: [.bug, .water])
let golisopod = Pokemon(name: "Golisopod", types: [.bug, .water])
let sandygast = Pokemon(name: "Sandygast", types: [.ghost, .ground])
let palossand = Pokemon(name: "Palossand", types: [.ghost, .ground])
let pyukumuku = Pokemon(name: "Pyukumuku", types: [.water])
let type_null = Pokemon(name: "Type: Null", types: [.normal])
let silvally = Pokemon(name: "Silvally", types: [.normal])
let minior = Pokemon(name: "Minior", types: [.rock, .flying])
let komala = Pokemon(name: "Komala", types: [.normal])
let turtonator = Pokemon(name: "Turtonator", types: [.fire, .dragon])
let togedemaru = Pokemon(name: "Togedemaru", types: [.electric, .steel])
let mimikyu = Pokemon(name: "Mimikyu", types: [.ghost, .fairy])
let bruxish = Pokemon(name: "Bruxish", types: [.water, .psychic])
let drampa = Pokemon(name: "Drampa", types: [.normal, .dragon])
let dhelmise = Pokemon(name: "Dhelmise", types: [.ghost, .grass])
let jangmo_o = Pokemon(name: "Jangmo-o", types: [.dragon])
let hakamo_o = Pokemon(name: "Hakamo-o", types: [.dragon, .fighting])
let kommo_o = Pokemon(name: "Kommo-o", types: [.dragon, .fighting])
let tapu_koko = Pokemon(name: "Tapu Koko", types: [.electric, .fairy])
let tapu_lele = Pokemon(name: "Tapu Lele", types: [.psychic, .fairy])
let tapu_bulu = Pokemon(name: "Tapu Bulu", types: [.grass, .fairy])
let tapu_fini = Pokemon(name: "Tapu Fini", types: [.water, .fairy])
let cosmog = Pokemon(name: "Cosmog", types: [.psychic])
let cosmoem = Pokemon(name: "Cosmoem", types: [.psychic])
let solgaleo = Pokemon(name: "Solgaleo", types: [.psychic, .steel])
let lunala = Pokemon(name: "Lunala", types: [.psychic, .ghost])
let nihilego = Pokemon(name: "Nihilego", types: [.rock, .poison])
let buzzwole = Pokemon(name: "Buzzwole", types: [.bug, .fighting])
let pheromosa = Pokemon(name: "Pheromosa", types: [.bug, .fighting])
let xurkitree = Pokemon(name: "Xurkitree", types: [.electric])
let celesteela = Pokemon(name: "Celesteela", types: [.steel, .flying])
let kartana = Pokemon(name: "Kartana", types: [.grass, .steel])
let guzzlord = Pokemon(name: "Guzzlord", types: [.dark, .dragon])
let necrozma = Pokemon(name: "Necrozma", types: [.psychic])
let magearna = Pokemon(name: "Magearna", types: [.steel, .fairy])
let marshadow = Pokemon(name: "Marshadow", types: [.fighting, .ghost])


let pokemons = [
     bulbasaur,
     ivysaur,
     venusaur,
     charmander,
     charmeleon,
     charizard,
     squirtle,
     wartortle,
     blastoise,
     caterpie,
     metapod,
     butterfree,
     weedle,
     kakuna,
     beedrill,
     pidgey,
     pidgeotto,
     pidgeot,
     rattata,
     raticate,
     spearow,
     fearow,
     ekans,
     arbok,
     pikachu,
     raichu,
     sandshrew,
     sandslash,
     nidoranF,
     nidorina,
     nidoqueen,
     nidoranM,
     nidorino,
     nidoking,
     clefairy,
     clefable,
     vulpix,
     ninetales,
     jigglypuff,
     wigglytuff,
     zubat,
     golbat,
     oddish,
     gloom,
     vileplume,
     paras,
     parasect,
     venonat,
     venomoth,
     diglett,
     dugtrio,
     meowth,
     persian,
     psyduck,
     golduck,
     mankey,
     primeape,
     growlithe,
     arcanine,
     poliwag,
     poliwhirl,
     poliwrath,
     abra,
     kadabra,
     alakazam,
     machop,
     machoke,
     machamp,
     bellsprout,
     weepinbell,
     victreebel,
     tentacool,
     tentacruel,
     geodude,
     graveler,
     golem,
     ponyta,
     rapidash,
     slowpoke,
     slowbro,
     magnemite,
     magneton,
     farfetchd,
     doduo,
     dodrio,
     seel,
     dewgong,
     grimer,
     muk,
     shellder,
     cloyster,
     gastly,
     haunter,
     gengar,
     onix,
     drowzee,
     hypno,
     krabby,
     kingler,
     voltorb,
     electrode,
     exeggcute,
     exeggutor,
     cubone,
     marowak,
     hitmonlee,
     hitmonchan,
     lickitung,
     koffing,
     weezing,
     rhyhorn,
     rhydon,
     chansey,
     tangela,
     kangaskhan,
     horsea,
     seadra,
     goldeen,
     seaking,
     staryu,
     starmie,
     mr_mime,
     scyther,
     jynx,
     electabuzz,
     magmar,
     pinsir,
     tauros,
     magikarp,
     gyarados,
     lapras,
     ditto,
     eevee,
     vaporeon,
     jolteon,
     flareon,
     porygon,
     omanyte,
     omastar,
     kabuto,
     kabutops,
     aerodactyl,
     snorlax,
     articuno,
     zapdos,
     moltres,
     dratini,
     dragonair,
     dragonite,
     mewtwo,
     mew,
     chikorita,
     bayleef,
     meganium,
     cyndaquil,
     quilava,
     typhlosion,
     totodile,
     croconaw,
     feraligatr,
     sentret,
     furret,
     hoothoot,
     noctowl,
     ledyba,
     ledian,
     spinarak,
     ariados,
     crobat,
     chinchou,
     lanturn,
     pichu,
     cleffa,
     igglybuff,
     togepi,
     togetic,
     natu,
     xatu,
     mareep,
     flaaffy,
     ampharos,
     bellossom,
     marill,
     azumarill,
     sudowoodo,
     politoed,
     hoppip,
     skiploom,
     jumpluff,
     aipom,
     sunkern,
     sunflora,
     yanma,
     wooper,
     quagsire,
     espeon,
     umbreon,
     murkrow,
     slowking,
     misdreavus,
     unown,
     wobbuffet,
     girafarig,
     pineco,
     forretress,
     dunsparce,
     gligar,
     steelix,
     snubbull,
     granbull,
     qwilfish,
     scizor,
     shuckle,
     heracross,
     sneasel,
     teddiursa,
     ursaring,
     slugma,
     magcargo,
     swinub,
     piloswine,
     corsola,
     remoraid,
     octillery,
     delibird,
     mantine,
     skarmory,
     houndour,
     houndoom,
     kingdra,
     phanpy,
     donphan,
     porygon2,
     stantler,
     smeargle,
     tyrogue,
     hitmontop,
     smoochum,
     elekid,
     magby,
     miltank,
     blissey,
     raikou,
     entei,
     suicune,
     larvitar,
     pupitar,
     tyranitar,
     lugia,
     ho_oh,
     celebi,
     treecko,
     grovyle,
     sceptile,
     torchic,
     combusken,
     blaziken,
     mudkip,
     marshtomp,
     swampert,
     poochyena,
     mightyena,
     zigzagoon,
     linoone,
     wurmple,
     silcoon,
     beautifly,
     cascoon,
     dustox,
     lotad,
     lombre,
     ludicolo,
     seedot,
     nuzleaf,
     shiftry,
     taillow,
     swellow,
     wingull,
     pelipper,
     ralts,
     kirlia,
     gardevoir,
     surskit,
     masquerain,
     shroomish,
     breloom,
     slakoth,
     vigoroth,
     slaking,
     nincada,
     ninjask,
     shedinja,
     whismur,
     loudred,
     exploud,
     makuhita,
     hariyama,
     azurill,
     nosepass,
     skitty,
     delcatty,
     sableye,
     mawile,
     aron,
     lairon,
     aggron,
     meditite,
     medicham,
     electrike,
     manectric,
     plusle,
     minun,
     volbeat,
     illumise,
     roselia,
     gulpin,
     swalot,
     carvanha,
     sharpedo,
     wailmer,
     wailord,
     numel,
     camerupt,
     torkoal,
     spoink,
     grumpig,
     spinda,
     trapinch,
     vibrava,
     flygon,
     cacnea,
     cacturne,
     swablu,
     altaria,
     zangoose,
     seviper,
     lunatone,
     solrock,
     barboach,
     whiscash,
     corphish,
     crawdaunt,
     baltoy,
     claydol,
     lileep,
     cradily,
     anorith,
     armaldo,
     feebas,
     milotic,
     castform,
     kecleon,
     shuppet,
     banette,
     duskull,
     dusclops,
     tropius,
     chimecho,
     absol,
     wynaut,
     snorunt,
     glalie,
     spheal,
     sealeo,
     walrein,
     clamperl,
     huntail,
     gorebyss,
     relicanth,
     luvdisc,
     bagon,
     shelgon,
     salamence,
     beldum,
     metang,
     metagross,
     regirock,
     regice,
     registeel,
     latias,
     latios,
     kyogre,
     groudon,
     rayquaza,
     jirachi,
     deoxys,
     turtwig,
     grotle,
     torterra,
     chimchar,
     monferno,
     infernape,
     piplup,
     prinplup,
     empoleon,
     starly,
     staravia,
     staraptor,
     bidoof,
     bibarel,
     kricketot,
     kricketune,
     shinx,
     luxio,
     luxray,
     budew,
     roserade,
     cranidos,
     rampardos,
     shieldon,
     bastiodon,
     burmy,
     wormadam,
     mothim,
     combee,
     vespiquen,
     pachirisu,
     buizel,
     floatzel,
     cherubi,
     cherrim,
     shellos,
     gastrodon,
     ambipom,
     drifloon,
     drifblim,
     buneary,
     lopunny,
     mismagius,
     honchkrow,
     glameow,
     purugly,
     chingling,
     stunky,
     skuntank,
     bronzor,
     bronzong,
     bonsly,
     mime_jr,
     happiny,
     chatot,
     spiritomb,
     gible,
     gabite,
     garchomp,
     munchlax,
     riolu,
     lucario,
     hippopotas,
     hippowdon,
     skorupi,
     drapion,
     croagunk,
     toxicroak,
     carnivine,
     finneon,
     lumineon,
     mantyke,
     snover,
     abomasnow,
     weavile,
     magnezone,
     lickilicky,
     rhyperior,
     tangrowth,
     electivire,
     magmortar,
     togekiss,
     yanmega,
     leafeon,
     glaceon,
     gliscor,
     mamoswine,
     porygon_z,
     gallade,
     probopass,
     dusknoir,
     froslass,
     rotom,
     uxie,
     mesprit,
     azelf,
     dialga,
     palkia,
     heatran,
     regigigas,
     giratina,
     cresselia,
     phione,
     manaphy,
     darkrai,
     shaymin,
     arceus,
     victini,
     snivy,
     servine,
     serperior,
     tepig,
     pignite,
     emboar,
     oshawott,
     dewott,
     samurott,
     patrat,
     watchog,
     lillipup,
     herdier,
     stoutland,
     purrloin,
     liepard,
     pansage,
     simisage,
     pansear,
     simisear,
     panpour,
     simipour,
     munna,
     musharna,
     pidove,
     tranquill,
     unfezant,
     blitzle,
     zebstrika,
     roggenrola,
     boldore,
     gigalith,
     woobat,
     swoobat,
     drilbur,
     excadrill,
     audino,
     timburr,
     gurdurr,
     conkeldurr,
     tympole,
     palpitoad,
     seismitoad,
     throh,
     sawk,
     sewaddle,
     swadloon,
     leavanny,
     venipede,
     whirlipede,
     scolipede,
     cottonee,
     whimsicott,
     petilil,
     lilligant,
     basculin,
     sandile,
     krokorok,
     krookodile,
     darumaka,
     darmanitan,
     maractus,
     dwebble,
     crustle,
     scraggy,
     scrafty,
     sigilyph,
     yamask,
     cofagrigus,
     tirtouga,
     carracosta,
     archen,
     archeops,
     trubbish,
     garbodor,
     zorua,
     zoroark,
     minccino,
     cinccino,
     gothita,
     gothorita,
     gothitelle,
     solosis,
     duosion,
     reuniclus,
     ducklett,
     swanna,
     vanillite,
     vanillish,
     vanilluxe,
     deerling,
     sawsbuck,
     emolga,
     karrablast,
     escavalier,
     foongus,
     amoonguss,
     frillish,
     jellicent,
     alomomola,
     joltik,
     galvantula,
     ferroseed,
     ferrothorn,
     klink,
     klang,
     klinklang,
     tynamo,
     eelektrik,
     eelektross,
     elgyem,
     beheeyem,
     litwick,
     lampent,
     chandelure,
     axew,
     fraxure,
     haxorus,
     cubchoo,
     beartic,
     cryogonal,
     shelmet,
     accelgor,
     stunfisk,
     mienfoo,
     mienshao,
     druddigon,
     golett,
     golurk,
     pawniard,
     bisharp,
     bouffalant,
     rufflet,
     braviary,
     vullaby,
     mandibuzz,
     heatmor,
     durant,
     deino,
     zweilous,
     hydreigon,
     larvesta,
     volcarona,
     cobalion,
     terrakion,
     virizion,
     tornadus,
     thundurus,
     reshiram,
     zekrom,
     landorus,
     kyurem,
     keldeo,
     meloetta,
     genesect,
     chespin,
     quilladin,
     chesnaught,
     fennekin,
     braixen,
     delphox,
     froakie,
     frogadier,
     greninja,
     bunnelby,
     diggersby,
     fletchling,
     fletchinder,
     talonflame,
     scatterbug,
     spewpa,
     vivillon,
     litleo,
     pyroar,
     flabébé,
     floette,
     florges,
     skiddo,
     gogoat,
     pancham,
     pangoro,
     furfrou,
     espurr,
     meowstic,
     honedge,
     doublade,
     aegislash,
     spritzee,
     aromatisse,
     swirlix,
     slurpuff,
     inkay,
     malamar,
     binacle,
     barbaracle,
     skrelp,
     dragalge,
     clauncher,
     clawitzer,
     helioptile,
     heliolisk,
     tyrunt,
     tyrantrum,
     amaura,
     aurorus,
     sylveon,
     hawlucha,
     dedenne,
     carbink,
     goomy,
     sliggoo,
     goodra,
     klefki,
     phantump,
     trevenant,
     pumpkaboo,
     gourgeist,
     bergmite,
     avalugg,
     noibat,
     noivern,
     xerneas,
     yveltal,
     zygarde,
     diancie,
     hoopa,
     volcanion,
     rowlet,
     dartrix,
     decidueye,
     litten,
     torracat,
     incineroar,
     popplio,
     brionne,
     primarina,
     pikipek,
     trumbeak,
     toucannon,
     yungoos,
     gumshoos,
     grubbin,
     charjabug,
     vikavolt,
     crabrawler,
     crabominable,
     oricorio,
     cutiefly,
     ribombee,
     rockruff,
     lycanroc,
     wishiwashi,
     mareanie,
     toxapex,
     mudbray,
     mudsdale,
     dewpider,
     araquanid,
     fomantis,
     lurantis,
     morelull,
     shiinotic,
     salandit,
     salazzle,
     stufful,
     bewear,
     bounsweet,
     steenee,
     tsareena,
     comfey,
     oranguru,
     passimian,
     wimpod,
     golisopod,
     sandygast,
     palossand,
     pyukumuku,
     type_null,
     silvally,
     minior,
     komala,
     turtonator,
     togedemaru,
     mimikyu,
     bruxish,
     drampa,
     dhelmise,
     jangmo_o,
     hakamo_o,
     kommo_o,
     tapu_koko,
     tapu_lele,
     tapu_bulu,
     tapu_fini,
     cosmog,
     cosmoem,
     solgaleo,
     lunala,
     nihilego,
     buzzwole,
     pheromosa,
     xurkitree,
     celesteela,
     kartana,
     guzzlord,
     necrozma,
     magearna,
     marshadow,
]
