import * as RxJS from 'rxjs';
import { overlapArea, Rectangle } from './overlap';

const unexpected = () => {
  throw new Error("unexpected");
}

interface Claim extends Rectangle {
  id: number;
}

function *claimDefinitions(defs: string): IterableIterator<Claim> {
  for (const text of defs.split('\n')) {
    if (text.length < 1) continue;
    const matches = text.match(/^#(\d+) @ (\d+),(\d+): (\d+)x(\d+)$/);
    if (!matches) {
      console.log('failed parsing', text);
      continue;
    }

    yield {
      id: parseInt(matches[1]),
      x: parseInt(matches[2]),
      y: parseInt(matches[3]),
      w: parseInt(matches[4]),
      h: parseInt(matches[5]),
    };
  }
}

const root = document.getElementById('root') as HTMLCanvasElement;
const ctx = root.getContext('2d') || unexpected();

const button = document.getElementById('button') as HTMLButtonElement;
const textarea = document.getElementById('stuffs') as HTMLTextAreaElement;

RxJS.fromEvent(button, 'click')
  .subscribe(() => {
    ctx.fillStyle = '#3c3836';
    ctx.fillRect(0, 0, root.width, root.height);

    const ids = [] as (Claim | null)[];
    const claims = [] as Claim[];

    for (const claim of claimDefinitions(textarea.value)) {
      ctx.fillStyle = 'rgba(235, 219, 178, .5)';
      ctx.fillRect(claim.x, claim.y, claim.w, claim.h);
      claims.push(claim);
      ids[claim.id] = claim;
    }

    for (const c1 of claims) {
      for (const c2 of claims) {
        if (c1 === c2) continue;

        const ov = overlapArea(c1, c2);
        if (!ov) continue;
        ids[c1.id] = null;
        ids[c2.id] = null;
      }
    }

    const nonOverlapping = ids.filter((x) => x) as Claim[];

    for (const claim of nonOverlapping) {
      ctx.fillStyle = '#b8bb26';
      ctx.fillRect(claim.x, claim.y, claim.w, claim.h);
      ctx.fillStyle = '#dbd1c7';
      ctx.fillText(claim.id.toString(), claim.x, claim.y + 7);
    }
  });
