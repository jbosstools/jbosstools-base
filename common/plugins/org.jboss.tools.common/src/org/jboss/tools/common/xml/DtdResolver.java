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
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.HashSet;
import java.util.Set;

import org.eclipse.wst.xml.core.internal.XMLCorePlugin;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import org.jboss.tools.common.CommonPlugin;
import org.jboss.tools.common.util.HttpUtil;

/**
 * @author igels
 */
public class DtdResolver implements EntityResolver {
	static Set unfound = new HashSet();

    public InputStream getInputStream(String publicId, String systemId) throws SAXException, IOException {
        String location = XMLCorePlugin.getDefault().getDefaultXMLCatalog().resolvePublic(publicId, systemId);
        if(location == null) location = XMLCorePlugin.getDefault().getDefaultXMLCatalog().resolveSystem(systemId);
        if(location == null) location = XMLCorePlugin.getDefault().getDefaultXMLCatalog().resolveURI(systemId);
        if(location == null) {
        	if(systemId != null && systemId.startsWith("file:") && systemId.endsWith(".xsd")) {
        		int i = systemId.replace('\\', '/').lastIndexOf('/');
        		String systemId2 = systemId.substring(i + 1);
        		location = XMLCorePlugin.getDefault().getDefaultXMLCatalog().resolveURI(systemId2);
        	}
        	
        }
        if(location == null) {
        	if(systemId != null && !unfound.contains(systemId)) {
        		unfound.add(systemId);
            	System.out.println("Cannot find locally:" );
            	System.out.println("Public ID " + publicId);
            	System.out.println("System ID " + systemId);
        	}
        }
        if(location!=null) {
            try {
            	URL url = new URL(location);
	            File file = new File(url.getFile());
	            if(file.isFile()) {
	                return new FileInputStream(file);
	            } else if("jar".equals(url.getProtocol())) {
	            	return url.openStream();
	            }
            } catch(Exception e) {
            	CommonPlugin.log("Error in DtdResolver: " + e.getMessage());
            }
        }

        String resourceType = null;
        if(systemId!=null) {
	        if(systemId.toLowerCase().endsWith(".dtd")) {
	            resourceType = "DTD";
	        } else if(systemId.toLowerCase().endsWith(".xsd")) {
	            resourceType = "XSD";
	        }
        }
        InputStream is = null;
        if(resourceType!=null) { // this deactivates DTD and XSD
            try {
            	URL url = new URL(systemId);
            	if("http".equals(url.getProtocol())) { 
            		is = HttpUtil.getInputStreamFromUrlByGetMethod(systemId);
            	}
    		} catch (Exception e) {
    			CommonPlugin.log(e.getMessage());
    			// don't handle any exeptions. Bug #ESL-306
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