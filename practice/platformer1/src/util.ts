import kaplay from "kaplay";

const k = kaplay();
export const pickAnim = (entity, animName = "idle") => {
  if (entity.isJumping()) animName = "jump";
  if (entity.isFalling()) animName = "fall";
  const oldFlipX = entity.flipX;
  if (entity.isRolling && entity.sprite !== entity.rollSprite)
    entity.use(k.sprite(entity.rollSprite));
  if (!entity.isRolling && entity.sprite !== entity.baseSprite)
    entity.use(k.sprite(entity.baseSprite));
  if (!entity.hasAnim(animName)) entity.use(k.sprite(entity.baseSprite));
  entity.flipX = oldFlipX;
  if (entity.curAnim() != animName) entity.play(animName);
  console.log(entity.curAnim(), entity.sprite);
};
