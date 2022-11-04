import Link from "next/link";
import { ArrowRightCircle } from "react-feather";

export function StudentRowItem({ name, url }: { name: string; url: string }) {
    return (
        <Link href={url}>
            <div
                className={
                    "p-3 m-3 text-xl group hover:bg-zinc-100 rounded-lg flex flex-row align-middle cursor-pointer justify-between transition-all duration-500 ease-in-out"
                }
            >
                <span>{name}</span>
                <ArrowRightCircle
                    size={30}
                    className="cursor-pointer group-hover:scale-125 transition-transform duration-500 ease-in-out"
                />
            </div>
        </Link>
    );
}
