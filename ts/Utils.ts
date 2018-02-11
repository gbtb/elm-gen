export type GenCommand = Decoders | Encoders | DecodersAndEncoders;

export class Decoders{
    Decoders: string =  ""
}

export class Encoders{
    Encoders: string =  ""
}

export class DecodersAndEncoders{
    DecodersAndEncoders: string =  ""
}


export function parseGenCommand(str: string) : GenCommand
{
    if (str.match(/^d\w*[\W]e\w*$/i) || str.match(/e\w*[\W]d\w*$/i))
        return new DecodersAndEncoders();
    
    if (str.match(/^d\w*$/i))
        return new Decoders();

    if (str.match(/^e\w*$/i))
        return new Encoders();

    throw new Error("Cannot parse generation command!");
}

export function getOutputFileName(inputFileName: string, com: GenCommand): string
{
    return `${inputFileName}${com.constructor.name}.elm`;
}