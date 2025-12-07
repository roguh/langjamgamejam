(
    (k.loadRoot "./")
    (k.loadSprite "bean" "sprites/bean.png")
    (k.add '(
        (k.pos 120 80) (k.sprite "bean")
    ))
    (k.onClick (lambda () (
        (k.addKaboom (k.mousePos))
    ))
)
