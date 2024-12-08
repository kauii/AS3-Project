module Assets.RandomEntities.RandomizedEnemies () where

import Types

goblin :: Enemy
goblin = Enemy
    { enemyId = Nothing
    , enemyName = "Goblin"
    , enemyHealth = 30
    , enemyMaxHealth = 30
    , enemyAttack = 10
    , enemyDefense = 2
    , enemyAgility = 10
    , enemyDifficulty = Easy
    , loot = []
    }

-- Sample enemy
ghoul :: Enemy
ghoul = Enemy
    { enemyId = Nothing
    , enemyName = "Ghoul"
    , enemyHealth = 25
    , enemyMaxHealth = 25
    , enemyAttack = 5
    , enemyDefense = 10
    , enemyAgility = 21
    , enemyDifficulty = Easy
    , loot = []
    }

rat :: Enemy
rat = Enemy
    { enemyId = Nothing
    , enemyName = "Rat"
    , enemyHealth = 15
    , enemyMaxHealth = 15
    , enemyAttack = 3
    , enemyDefense = 1
    , enemyAgility = 12
    , enemyDifficulty = Easy
    , loot = []
    }

bandit :: Enemy
bandit = Enemy
    { enemyId = Nothing
    , enemyName = "Bandit"
    , enemyHealth = 35
    , enemyMaxHealth = 35
    , enemyAttack = 8
    , enemyDefense = 3
    , enemyAgility = 9
    , enemyDifficulty = Easy
    , loot = []
    }

skeletalMinion :: Enemy
skeletalMinion = Enemy
    { enemyId = Nothing
    , enemyName = "Skeletal Minion"
    , enemyHealth = 20
    , enemyMaxHealth = 20
    , enemyAttack = 7
    , enemyDefense = 5
    , enemyAgility = 8
    , enemyDifficulty = Easy
    , loot = []
    }

orc :: Enemy
orc = Enemy
    { enemyId = Nothing
    , enemyName = "Orc"
    , enemyHealth = 50
    , enemyMaxHealth = 50
    , enemyAttack = 15
    , enemyDefense = 7
    , enemyAgility = 8
    , enemyDifficulty = Normal
    , loot = []
    }

darkKnight :: Enemy
darkKnight = Enemy
    { enemyId = Nothing
    , enemyName = "Dark Knight"
    , enemyHealth = 60
    , enemyMaxHealth = 60
    , enemyAttack = 18
    , enemyDefense = 12
    , enemyAgility = 10
    , enemyDifficulty = Normal
    , loot = []
    }

troll :: Enemy
troll = Enemy
    { enemyId = Nothing
    , enemyName = "Troll"
    , enemyHealth = 70
    , enemyMaxHealth = 70
    , enemyAttack = 20
    , enemyDefense = 10
    , enemyAgility = 6
    , enemyDifficulty = Normal
    , loot = []
    }

draconicWarrior :: Enemy
draconicWarrior = Enemy
    { enemyId = Nothing
    , enemyName = "Draconic Warrior"
    , enemyHealth = 150
    , enemyMaxHealth = 150
    , enemyAttack = 35
    , enemyDefense = 20
    , enemyAgility = 12
    , enemyDifficulty = Hard
    , loot = []
    }

fallenPaladin :: Enemy
fallenPaladin = Enemy
    { enemyId = Nothing
    , enemyName = "Fallen Paladin"
    , enemyHealth = 130
    , enemyMaxHealth = 130
    , enemyAttack = 30
    , enemyDefense = 25
    , enemyAgility = 14
    , enemyDifficulty = Hard
    , loot = []
    }

wraithLord :: Enemy
wraithLord = Enemy
    { enemyId = Nothing
    , enemyName = "Wraith Lord"
    , enemyHealth = 110
    , enemyMaxHealth = 110
    , enemyAttack = 32
    , enemyDefense = 18
    , enemyAgility = 20
    , enemyDifficulty = Hard
    , loot = []
    }