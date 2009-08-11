/*******************************************************************************
 * Copyright (c) 2007 Exadel, Inc. and Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Exadel, Inc. and Red Hat, Inc. - initial API and implementation
 ******************************************************************************/ 
package org.jboss.tools.common.xml;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.Platform;
import org.eclipse.wst.xml.core.internal.XMLCorePlugin;
import org.jboss.tools.common.CommonPlugin;
import org.jboss.tools.common.util.HttpUtil;
import org.osgi.framework.Bundle;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

/**
 * @author igels
 */
public class DtdResolver implements EntityResolver {
	static Set unfound = new HashSet();

    public InputStream getInputStream(String publicId, String systemId) throws SAXException, IOException {
        String location = XMLCorePlugin.getDefault().getDefaultXMLCatalog().resolvePublic(publicId, systemId);
        if(location == null) {
			location = XMLCorePlugin.getDefault().getDefaultXMLCatalog().resolveSystem(systemId);
		}
        if(location == null) {
			location = XMLCorePlugin.getDefault().getDefaultXMLCatalog().resolveURI(systemId);
		}
        if(location == null) {
        	if(systemId != null && systemId.startsWith("file:") && systemId.endsWith(".xsd")) { //$NON-NLS-1$ //$NON-NLS-2$
        		int i = systemId.replace('\\', '/').lastIndexOf('/');
        		String systemId2 = systemId.substring(i + 1);
        		location = XMLCorePlugin.getDefault().getDefaultXMLCatalog().resolveURI(systemId2);
        	}
        	
        }
        if((location == null || location.startsWith("http:")) && systemId != null) { //$NON-NLS-1$
        	Bundle b = Platform.getBundle("org.eclipse.jst.standard.schemas"); //$NON-NLS-1$
        	if(b != null) {
        		int q = systemId.lastIndexOf("/"); //$NON-NLS-1$
        		String s = systemId.substring(q + 1);
        		URL u = b.getEntry("/dtdsAndSchemas/" + s); //$NON-NLS-1$
        		try {
        			if(u != null) {
						u = FileLocator.resolve(u);
					}
        		} catch (IOException ee) {
        			u = null;
        		}
        		if(u != null) {
					location = u.toString();
				}
        	}
        }
        if(location == null) {
        	if(systemId != null && !unfound.contains(systemId)) {
        		unfound.add(systemId);
            	CommonPlugin.getPluginLog().logError("Cannot find locally: "  //$NON-NLS-1$
            			+ "Public ID " + publicId //$NON-NLS-1$
            			+ " System ID " + systemId); //$NON-NLS-1$
        	}
        }
        if(location!=null) {
            try {
            	URL url = new URL(location);
	            File file = new File(url.getFile());
	            if(file.isFile()) {
	                return new FileInputStream(file);
	            } else if("jar".equals(url.getProtocol())) { //$NON-NLS-1$
	            	return url.openStream();
	            }
            } catch(FileNotFoundException e) {
    			CommonPlugin.getPluginLog().logError("Error in DtdResolver: " + e.getMessage()); //$NON-NLS-1$
            }
        }

        String resourceType = null;
        if(systemId!=null) {
	        if(systemId.toLowerCase().endsWith(".dtd")) { //$NON-NLS-1$
	            resourceType = "DTD"; //$NON-NLS-1$
	        } else if(systemId.toLowerCase().endsWith(".xsd")) { //$NON-NLS-1$
	            resourceType = "XSD"; //$NON-NLS-1$
	        } else if(systemId.toLowerCase().endsWith(".ent")) { //$NON-NLS-1$
	            resourceType = "ENT"; //$NON-NLS-1$
	        }
        }
        InputStream is = null;
        if(resourceType!=null) { // this deactivates DTD and XSD
            try {
            	URL url = new URL(systemId);
            	if("http".equals(url.getProtocol())) {  //$NON-NLS-1$
            		is = HttpUtil.getInputStreamFromUrlByGetMethod(systemId);
            	}
    		} catch (MalformedURLException e) {
      			CommonPlugin.getPluginLog().logError( e.getMessage());
    			// don't handle any exeptions. Bug #ESL-306
            } catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
        return is;
    }

    public InputSource resolveEntity(String publicId, String systemId) throws SAXException, IOException {
        InputStream is = getInputStream(publicId, systemId);
	    if(is!=null) {
	        return new InputSource(is);
	    }
		return null;
    }
}