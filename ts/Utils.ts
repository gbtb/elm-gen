export enum GenCommand {
    Decoders = "Decoders",
    Encoders = "Encoders",
    DecodersAndEncoders = "DecodersAndEncoders"
}

export function parseGenCommand(str: string) : GenCommand
{
    if (str.match(/^d\w*[\W]e\w*$/i) || str.match(/e\w*[\W]d\w*$/i))
        return GenCommand.DecodersAndEncoders;
    
    if (str.match(/^d\w*$/i))
        return GenCommand.Decoders;

    if (str.match(/^e\w*$/i))
        return GenCommand.Encoders;

    throw new Error("Cannot parse generation command!");
}

export function getOutputFileName(inputFileName: string, com: GenCommand): string
{
    return `${inputFileName}${com}.elm`;
}