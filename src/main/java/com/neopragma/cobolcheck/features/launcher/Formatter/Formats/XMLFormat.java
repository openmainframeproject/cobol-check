package com.neopragma.cobolcheck.features.launcher.Formatter.Formats;

import com.neopragma.cobolcheck.features.launcher.Formatter.DataTransferObjects.DataTransferObjectStyle;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import java.io.File;

public class XMLFormat extends Formatter{

    public XMLFormat(DataTransferObjectStyle dataTransferObjectStyle){
        super(dataTransferObjectStyle);
    }

    @Override
    public String writeInFormat(String path) throws JAXBException {
        Object dataTransferObject = this.dataTransferObject.getDataTransferObject();
        JAXBContext context = JAXBContext.newInstance(dataTransferObject.getClass());
        Marshaller mar= context.createMarshaller();
        mar.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
        mar.marshal(dataTransferObject, new File(path));
        return mar.toString();
    }
}
