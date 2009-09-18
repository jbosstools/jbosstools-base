/*
 * XMLEntityResolver.java
 * Created on February 21, 2003, 9:38 AM
 */

package org.jboss.tools.common.xml;
import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.net.URL;
import java.util.Properties;

import org.eclipse.core.runtime.FileLocator;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * @author  valera
 */
public class XMLEntityResolver implements EntityResolver {

    private static final Properties publicEntities = new Properties();
	private static final Properties systemEntities = new Properties();

    public static void registerPublicEntity(String publicId, String url) {
		publicEntities.setProperty(publicId, url);
    }

    public static void registerPublicEntity(String publicId, Class<?> loader, String resourceName) throws IOException {
    	URL url = resolve(loader, resourceName);
    	if(url != null) {
			registerPublicEntity(publicId, url.toString());
		}
    }

	public static void registerSystemEntity(String systemId, String url) {
		systemEntities.setProperty(systemId, url);
	}

    public static void registerSystemEntity(String systemId, Class<?> loader, String resourceName) throws IOException {
    	URL url = resolve(loader, resourceName);
    	if(url != null) {
			registerSystemEntity(systemId, url.toString());
		}
    }
    
    static URL resolve(Class<?> loader, String resourceName) throws IOException {
    	URL url = loader.getResource(resourceName);
    	return (url == null) ? null : FileLocator.resolve(url);
    }

    public static XMLEntityResolver getInstance() {
        return new XMLEntityResolver();
    }
    
    boolean deactivate = true;

    private XMLEntityResolver() {}
    
    public void setDeactivate(boolean b) {
    	deactivate = b;
    }
    
    public boolean isResolved(String publicId, String systemId) {
    	if (publicId != null) {
			String url = publicEntities.getProperty(publicId);
			if (url != null) {
				return true;
			}
    	} else if (systemId != null) {
			String url = systemEntities.getProperty(systemId);
			if (url != null) {
				return true;
			}
        }
		return false;
    }

    public InputSource resolveEntity(String publicId, String systemId) 
    	throws SAXException, java.io.IOException {
		InputSource source = null;
		boolean ok = false;
    	if (publicId != null) {
			String url = publicEntities.getProperty(publicId);
			if (url != null) {
				source = new InputSource(url);
				source.setPublicId(publicId);
				ok = true;
			}
    	}
    	if (!ok && systemId != null) {
			String url = systemEntities.getProperty(systemId);
			if (url != null) {
				source = new InputSource(url);
				source.setSystemId(systemId);
			}
        }

		if(deactivate && (systemId != null) && (source == null) && (systemId.toLowerCase().endsWith(".dtd"))) { // this deactivates DTD //$NON-NLS-1$
			source = new InputSource(new ByteArrayInputStream("<?xml version='1.0' encoding='UTF-8'?>".getBytes())); //$NON-NLS-1$
		}

        return source;
    }

}