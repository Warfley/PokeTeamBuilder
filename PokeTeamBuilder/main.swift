import Foundation

extension MutableCollection where Indices.Iterator.Element == Index {
    /// Shuffles the contents of this collection.
    mutating func shuffle() {
        let c = count
        guard c > 1 else { return }
        
        for (firstUnshuffled , unshuffledCount) in zip(indices, stride(from: c, to: 1, by: -1)) {
            let d: IndexDistance = numericCast(arc4random_uniform(numericCast(unshuffledCount)))
            guard d != 0 else { continue }
            let i = index(firstUnshuffled, offsetBy: d)
            swap(&self[firstUnshuffled], &self[i])
        }
    }
}

extension Sequence {
    /// Returns an array with the contents of this sequence, shuffled.
    func shuffled() -> [Iterator.Element] {
        var result = Array(self)
        result.shuffle()
        return result
    }
}


let strengths: [PokeType: [PokeType]] = [.normal: [],
                                        .fighting: [.normal, .rock, .ice, .steel, .dark],
                                        .flying: [.fighting, .bug, .grass],
                                        .poison: [.grass, .fairy],
                                        .ground:[.poison, .rock, .fire, .electric, .steel],
                                        .rock: [.flying, .bug, .fire, .ice],
                                        .bug: [.grass, .psychic, .dark],
                                        .ghost: [.ghost, .psychic],
                                        .steel: [.rock, .ice, .fairy],
                                        .fire: [.bug, .steel, .grass, .ice],
                                        .water: [.ground, .rock, .fire],
                                        .grass: [.ground, .rock, .water],
                                        .electric: [.flying, .water],
                                        .psychic: [.fighting, .poison],
                                        .ice: [.flying, .ground, .grass, .dragon],
                                        .dragon: [.dragon],
                                        .dark: [.ghost, .psychic],
                                        .fairy: [.fighting, .dragon, .dark]]


func iterateEnum<T: Hashable>(_: T.Type) -> AnyIterator<T> {
    var i = 0
    return AnyIterator {
        let next = withUnsafeBytes(of: &i) { $0.load(as: T.self) }
        if next.hashValue != i { return nil }
        i += 1
        return next
    }
}

let types = { () -> [PokeType] in
    var res = [PokeType]()
    for t in iterateEnum(PokeType.self) {
        res.append(t)
    }
    return res
}()

let weaknesses = { () -> [PokeType:[PokeType]] in
    var result = [PokeType:[PokeType]]()
    for t in iterateEnum(PokeType.self) {
        result[t] = [PokeType]()
        strengths.forEach( { (tt, w) in
            if w.contains(t) {
                result[t]!.append(tt)
            }
        } )
    }
    return result
}()

func findStrengths(team: [Pokemon]) -> [PokeType] {
    var res = [PokeType]()
    
    for p in team {
        for t in p.types {
            for tt in strengths[t]! {
                if !res.contains(tt) {
                    res.append(tt)
                }
            }
        }
    }
    return res
}

func removeStrengths(team: [Pokemon], types: [PokeType]) -> [PokeType] {
    var res = types
    let s = findStrengths(team: team)
    res = res.filter( {!s.contains($0)} )
    return res
}

func findWeakness(team: [Pokemon]) -> [PokeType] {
    var w = [PokeType]()
    
    for p in team {
        for t in p.types {
            for tt in weaknesses[t]! {
                if !w.contains(tt) {
                    w.append(tt)
                }
            }
        }
    }
    
    w = removeStrengths(team: team, types: w)
    
    return w
}

func findNeutral(team: [Pokemon]) -> [PokeType] {
    var res = [PokeType]()
    let s = findStrengths(team: team)
    let w = findWeakness(team: team)
    
    for t in iterateEnum(PokeType.self) {
        if (!s.contains(t) && !w.contains(t)) {
            res.append(t)
        }
    }
    
    return res
}

func genTeam(_ basePokemon: [Pokemon], _ pool: [Pokemon], _ count: Int) -> [Pokemon] {
    if basePokemon.count == count {
        return basePokemon
    }
    
    var team = basePokemon
    let pool = pool.shuffled()
    
    var tp = types
    
    tp = removeStrengths(team: team, types: tp)
    var min = tp.count
    var minP: Pokemon? = nil
    
    for p in pool {
        let t = removeStrengths(team: [p], types: tp).count
        if t < min {
            min = t
            minP = p
        }
    }
    
    team.append(minP ?? pool.first!)
    
    return genTeam(team, pool.filter( { $0.name != team.last?.name } ), count)
}


var c = 0
var reqTeam = [Pokemon]()
var pool = [Pokemon]()

if CommandLine.argc > 2 {
    switch CommandLine.arguments[1] {
    case "r":
        pool = redPokemon
    case "g":
        pool = greenPokemon
    case "b":
        pool = bluePokemon
    case "y":
        pool = yellowPokemon
    case "go":
        pool = goldPokemon
    case "si":
        pool = silverPokemon
    case "cr":
        pool = crystalPokemon
    default:
        print("unkown edition")
    }
    c = Int(CommandLine.arguments[2])!
    for i in 2..<CommandLine.argc {
        let s = CommandLine.arguments[Int(i)].lowercased()
        for p in yellowPokemon {
            if p.name.lowercased() == s {
                reqTeam.append(p)
                break
            }
        }
    }
} else if CommandLine.argc == 1 {
    var s: String;
    while pool.isEmpty {
        print("Enter edition shortcut (r, g, b, y, go, si, cr)")
        s = readLine()!
        switch s {
        case "r":
            pool = redPokemon
        case "g":
            pool = greenPokemon
        case "b":
            pool = bluePokemon
        case "y":
            pool = yellowPokemon
        case "go":
            pool = goldPokemon
        case "si":
            pool = silverPokemon
        case "cr":
            pool = crystalPokemon
        default:
            print("unkown edition")
        }
    }
    print("Enter amount of teams to generate")
    c = Int(readLine()!)!
    repeat {
        print("Enter pokemon to be in the team (empty if none)")
        s = (readLine() ?? "").lowercased()
        for p in yellowPokemon {
            if p.name.lowercased() == s {
                reqTeam.append(p)
                break
            }
        }
    } while !s.isEmpty
} else {
    print("Usage: PokeTeamBuilder E N [P1 P2 ...]")
    print("E: Edition (r, g, b, y, go, si, cr)")
    print("N: Number of teams to generate")
    print("PX: default pokemon name, e.g. raichu")
}

while true {
    var start = reqTeam.isEmpty ? [yellowPokemon.shuffled().first!] : reqTeam
    for s in start {
        pool = pool.filter({$0.name != s.name})
    }
    let t = genTeam(start, pool, 6)


    var s = ""
    for p in t { s = s + p.name + ", " }
    
    s = s.substring(to: s.index(s.endIndex, offsetBy: -2))

    let w = findWeakness(team: t).filter( { types.contains($0) } )
    
    let n = findNeutral(team: t).filter( { types.contains($0) } )
    print(s)
    print("weak against")
    print(w.map({ (p) -> String in
        return p.rawValue
    }))
    print("netral against")
    print(n.map({ (p) -> String in
        return p.rawValue
    }))
    print("-------------------------------------------------------------")
    c -= 1
    if c <= 0 { break }
}


