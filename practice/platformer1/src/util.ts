import kaplay from "kaplay";

const k = kaplay();
export const pickAnim = (entity, animName: string) => {
  if (entity.isJumping()) animName = "jump";
  if (entity.isFalling()) animName = "fall";
  const oldFlipX = entity.flipX;

  // Switch sprites?
  if (entity.isRolling && entity.sprite !== entity.rollSprite)
    entity.use(k.sprite(entity.rollSprite));
  if (!entity.isRolling && entity.sprite !== entity.baseSprite)
    entity.use(k.sprite(entity.baseSprite));

  if (!entity.hasAnim(animName)) {
    entity.use(k.sprite(entity.baseSprite));
  }

  // Preserve Direction if switching sprites
  entity.flipX = oldFlipX;

  // Only change if necessary
  if (entity.curAnim() != animName) entity.play(animName);
};

export const tileMaker =
  (name: string, health: number = undefined, anim: string = undefined) =>
  () => [
    "solid",
    k.sprite(name),
    k.area(),
    k.body({ isStatic: true }),
    { health },
  ];
