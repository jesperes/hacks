from mcpi.minecraft import Minecraft
from mcpi import block
from time import sleep

mc = Minecraft.create()
#mc.postToChat("Hello world")
#x, y, z = mc.player.getPos()
# mc.player.setPos(x, y, z)
# mc.setBlock(x + 1, y, z, block.GOLD_ORE.id)
#stone = 1
#mc.setBlocks(x + 1, y + 1, z + 1,
#             x + 11, y + 11, z + 11, stone)

grass = 2
flower = 38

while True:
    x, y, z = mc.player.getPos()
    block_beneath = mc.getBlock(x, y-1, z)
    if block_beneath != grass:
        mc.setBlock(x, y-1, z, grass)

    mc.setBlock(z, y-1, z, flower) 
        
    sleep(0.1)
