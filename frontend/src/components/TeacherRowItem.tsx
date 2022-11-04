import Link from "next/link";
import { ArrowRightCircle, Edit, Trash } from "react-feather";
import clsx from "clsx";

export function TeacherRowItem({
    name,
    url,
    editUrl,
    onClickTrash,
}: {
    name: string;
    url: string;
    editUrl: string;
    onClickTrash: () => void;
}) {
    return (
        <div
            className={clsx(
                "p-3 m-3 text-xl group hover:bg-zinc-100 rounded-lg flex flex-row align-middle justify-between transition-all duration-500 ease-in-out"
            )}
        >
            <span>{name}</span>
            <div className="flex flex-row gap-3">
                <Trash
                    size={30}
                    className="cursor-pointer"
                    onClick={onClickTrash}
                />
                <Link href={editUrl}>
                    <Edit size={30} className="cursor-pointer" />
                </Link>
                <Link href={url}>
                    <ArrowRightCircle size={30} className="cursor-pointer" />
                </Link>
            </div>
        </div>
    );
}
