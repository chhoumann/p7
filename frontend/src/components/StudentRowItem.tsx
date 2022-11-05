import Link from "next/link";
import { ArrowRightCircle, CheckCircle } from "react-feather";

export function StudentRowItem({
    name,
    url,
    showCheckmark,
}: {
    name: string;
    url: string;
    showCheckmark?: boolean;
}) {
    return (
        <Link href={url}>
            <div
                className={
                    "p-3 m-3 text-xl group hover:bg-zinc-100 rounded-lg flex flex-row align-middle cursor-pointer justify-between transition-all duration-200 ease-in-out"
                }
            >
                <span>{name}</span>
                {!showCheckmark ? (
                    <ArrowRightCircle
                        size={30}
                        className="cursor-pointer group-hover:scale-125 transition-transform duration-200 ease-in-out"
                    />
                ) : (
                    <CheckCircle
                        size={30}
                        className="text-green-500 cursor-pointer group-hover:scale-125 transition-transform duration-200 ease-in-out"
                    />
                )}
            </div>
        </Link>
    );
}
