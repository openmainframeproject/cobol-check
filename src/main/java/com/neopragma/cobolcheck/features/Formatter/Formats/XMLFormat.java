package com.neopragma.cobolcheck.features.Formatter.Formats;

import com.neopragma.cobolcheck.features.Formatter.DataTransferObjects.DataTransferObjectStyle;
import com.neopragma.cobolcheck.services.StringHelper;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import java.io.File;

public class XMLFormat extends Formatter{

    public XMLFormat(DataTransferObjectStyle dataTransferObjectStyle){
        super(dataTransferObjectStyle);
    }

    @Override
    public String getFormattedString(String path) throws JAXBException {
        Object dataTransferObject = dataTransferObjectAdapter.getDataTransferObject();
        JAXBContext context = JAXBContext.newInstance(dataTransferObject.getClass());
        Marshaller mar= context.createMarshaller();
        mar.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
        mar.marshal(dataTransferObject, new File(StringHelper.changeFileExtension(path, "xml")));
        return mar.toString();
    }
}
