/**
 * @deprecated
 */

package org.jboss.tools.common.model.util;

import org.xml.sax.*;

public class XMLEntityResolver implements EntityResolver {

    public static void registerPublicEntity(String publicId, String url) {
    	org.jboss.tools.common.xml.XMLEntityResolver.registerPublicEntity(publicId, url);
    }

	public static void registerSystemEntity(String systemId, String url) {
    	org.jboss.tools.common.xml.XMLEntityResolver.registerSystemEntity(systemId, url);
	}

    public static EntityResolver getInstance() {
        return new XMLEntityResolver();
    }
    
    EntityResolver impl;

    private XMLEntityResolver() {
    	impl = getInstance();
    }

    public InputSource resolveEntity(String publicId, String systemId) 
    	throws SAXException, java.io.IOException {
    	return impl.resolveEntity(publicId, systemId);
    }

    public InputSource resolveEntity2(String publicId, String systemId) 
        throws SAXException, java.io.IOException {
    	return impl.resolveEntity(publicId, systemId);
    }

}