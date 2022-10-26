import { NextPage } from 'next';
import { useRouter } from 'next/router'
import { trpc } from "../../utils/trpc"
import React, { useState } from 'react'
import Link from 'next/link';


const ExerciseId: NextPage = () => {

    const [selectedId, setSelectedId] = useState<string>()

    const router = useRouter();
    const { id } = router.query;
    //const sessions = trpc.useQuery
    const testData = [
        { name: "hello", id: "i21en" },
        { name: "Jesus", id: "i23en" },
        { name: "Christ", id: "i22en" },
        { name: "Christ", id: "i24en" },
        { name: "Christ", id: "i25en" },
        { name: "Christ", id: "i26en" },
        { name: "Christ", id: "i27en" },
        { name: "Christ", id: "i28en" },
        { name: "Christ", id: "i29en" },
        { name: "Christ", id: "i30en" },
        { name: "Christ", id: "i31en" },
    ]

    return (
        <div className='container flex justify-center items-center w-full h-[75vh]'>
            <div className='flex flex-col mt-40 w-[60vh] h-full border-solid border-2 border-gray-500 overflow-auto'>

                {testData.map((session) =>
                    <React.Fragment key={session.id}>
                        <ExerciseRow {...session} onClick={() => setSelectedId(session.id)} isSelected={session.id === selectedId}/>
                    </React.Fragment>
                )}
                <div className='mb-auto' />
                <div className='flex flex-row gap-4 mx-3 my-3 pt-3 pb-3 sticky bottom-0 bg-white'>
                    <Link href="/exercises/create">
                        <button className='bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black'>
                            Create new exercise
                        </button>
                    </Link>
                    <button className='bg-gray-300 px-3 py-2 hover:bg-gray-400 hover:outline hover:outline-2 hover:outline-black'>
                        Edit
                    </button>
                </div>
            </div>
        </div>
    )
}

export default ExerciseId;


function ExerciseRow({ id, name, isSelected, onClick }:{ id: string, name: string, isSelected: boolean, onClick: () => void }) {
    return (
        <div className={`p-3 m-3 ${isSelected?'bg-gray-400':"bg-gray-300 hover:bg-gray-400"}`} onClick={onClick}>
            <span>{name}</span>
        </div>
    )
}