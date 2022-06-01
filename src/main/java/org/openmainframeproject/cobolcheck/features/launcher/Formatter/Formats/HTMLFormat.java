package org.openmainframeproject.cobolcheck.features.launcher.Formatter.Formats;

import com.sun.org.apache.xalan.internal.xsltc.compiler.util.ErrorMsg;
import com.sun.org.apache.xalan.internal.xsltc.compiler.util.TypeCheckError;
import org.openmainframeproject.cobolcheck.features.launcher.Formatter.DataTransferObjects.DataTransferObject;
import org.openmainframeproject.cobolcheck.features.launcher.Formatter.DataTransferObjects.DataTransferObjectStyle;
import org.openmainframeproject.cobolcheck.services.filehelpers.EncodingIO;

import javax.swing.text.html.HTMLWriter;
import java.io.IOException;
import java.io.Writer;


public class HTMLFormat extends Formatter{
    public HTMLFormat(DataTransferObjectStyle dataTransferObjectStyle){
        super(dataTransferObjectStyle);
    }

    @Override
    public String writeInFormat(String path) throws IOException, TypeCheckError {
        Object dataTransferObject = this.dataTransferObject.getDataTransferObject();
        if (dataTransferObject instanceof String){
            String HTMLText = (String)dataTransferObject;
            Writer writer = EncodingIO.getWriterWithCorrectEncoding(path, false);
            writer.write(HTMLText);
            writer.close();
            return HTMLText;
        }
        throw new TypeCheckError(new ErrorMsg("Type of Data Transfer Object when writing HTML, must be <String>, " +
                "however; the given object could not be parsed as a String."));
    }
}
