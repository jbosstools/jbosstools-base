/*
 * XMLEntityResolver.java
 * Created on February 21, 2003, 9:38 AM
 */

package org.jboss.tools.common.xml;
import java.io.ByteArrayInputStream;
import java.util.Properties;
import org.xml.sax.*;

/**
 * @author  valera
 */
public class XMLEntityResolver implements EntityResolver {

    private static final Properties publicEntities = new Properties();
	private static final Properties systemEntities = new Properties();

    public static void registerPublicEntity(String publicId, String url) {
		publicEntities.setProperty(publicId, url);
    }

	public static void registerSystemEntity(String systemId, String url) {
		systemEntities.setProperty(systemId, url);
	}

    public static XMLEntityResolver getInstance() {
        return new XMLEntityResolver();
    }
    
    boolean deactivate = true;

    private XMLEntityResolver() {}
    
    public void setDeactivate(boolean b) {
    	deactivate = b;
    }

    public InputSource resolveEntity(String publicId, String systemId) 
    	throws SAXException, java.io.IOException {
		InputSource source = null;

    	if (publicId != null) {
			String url = publicEntities.getProperty(publicId);
			if (url != null) {
				source = new InputSource(url);
				source.setPublicId(publicId);
			}
    	} else if (systemId != null) {
			String url = systemEntities.getProperty(systemId);
			if (url != null) {
				source = new InputSource(url);
				source.setSystemId(systemId);
			}
        }

		if(deactivate && (systemId != null) && (source == null) && (systemId.toLowerCase().endsWith(".dtd"))) { // this deactivates DTD
			source = new InputSource(new ByteArrayInputStream("<?xml version='1.0' encoding='UTF-8'?>".getBytes()));
		}

        return source;
    }

}