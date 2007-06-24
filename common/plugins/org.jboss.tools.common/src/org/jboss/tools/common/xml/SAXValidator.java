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

import java.util.*;
import java.io.*;
import java.net.URL;
import org.apache.xerces.util.XMLCatalogResolver;
import org.apache.xerces.xni.*;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.osgi.framework.Bundle;
import org.xml.sax.*;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;
import org.jboss.tools.common.CommonPlugin;
import org.jboss.tools.common.util.FileUtil;

/**
 * @author eskimo(dgolovin@exadel.com)
 * @version   $Revision:$
 */
public class SAXValidator {
	
	protected static final String FATAL_ERROR_PROCESSING_FEATURE_ID 	= "http://apache.org/xml/features/continue-after-fatal-error";
	protected static final String ENTITY_RESOLVER_PROPERTY_ID 			= "http://apache.org/xml/properties/internal/entity-resolver";
	protected static final String NAMESPACES_FEATURE_ID 				= "http://xml.org/sax/features/namespaces";
    protected static final String NAMESPACE_PREFIXES_FEATURE_ID 		= "http://xml.org/sax/features/namespace-prefixes";
    protected static final String VALIDATION_FEATURE_ID 				= "http://xml.org/sax/features/validation";
    protected static final String VALIDATION_SCHEMA_FEATURE_ID 			= "http://apache.org/xml/features/validation/schema";
    protected static final String VALIDATION_SCHEMA_CHECKING_FEATURE_ID = "http://apache.org/xml/features/validation/schema-full-checking";
    protected static final String VALIDATION_DYNAMIC_FEATURE_ID 		= "http://apache.org/xml/features/validation/dynamic";
    
    protected static final String DEFAULT_SAX_PARSER_CLASS_NAME 		= "org.apache.xerces.parsers.SAXParser";

    /**
     * 
     * @return
     */
    XMLReader createParser() {
        DefaultHandler handler = new DefaultHandler();
        XMLReader parserInstance = null;

        try {
            parserInstance = XMLReaderFactory.createXMLReader(DEFAULT_SAX_PARSER_CLASS_NAME);
        } catch (Exception e) {
        	return null;
        }

        setFeature(parserInstance, NAMESPACES_FEATURE_ID, true);
        setFeature(parserInstance, NAMESPACE_PREFIXES_FEATURE_ID, false);
        setFeature(parserInstance, VALIDATION_FEATURE_ID, true);
        setFeature(parserInstance, VALIDATION_SCHEMA_FEATURE_ID, true);
        setFeature(parserInstance, VALIDATION_SCHEMA_CHECKING_FEATURE_ID, false);
        setFeature(parserInstance, VALIDATION_DYNAMIC_FEATURE_ID, false);
        setFeature(parserInstance, FATAL_ERROR_PROCESSING_FEATURE_ID, false);

        try {
            parserInstance.setProperty(ENTITY_RESOLVER_PROPERTY_ID, new XMLEntityResolverImpl());
        } catch (Exception e1) {
        	Status errorStatus = new Status(
        			Status.ERROR, CommonPlugin.PLUGIN_ID, 0, e1.getMessage()+"", e1);
        	CommonPlugin.getInstance().getLog().log(errorStatus);
        }
        
        parserInstance.setContentHandler(handler);
        parserInstance.setErrorHandler(handler);
        return parserInstance;
    }
    
    /**
     * 
     * @param parser
     * @param name
     * @param value
     */
    public static void setFeature(XMLReader parser, String name, boolean value) {
        try {
            parser.setFeature(name, value);
        } catch (SAXException e) {
        	// TODO - Move to NLS bundle
        	Status warningStatus = new Status(
        			Status.ERROR, CommonPlugin.PLUGIN_ID, 0, 
        			"warning: Parser does not support feature ("+name+")", e);
        	CommonPlugin.getInstance().getLog().log(warningStatus);
        }
    }

    /**
     * 
     * @param parser
     * @param name
     * @param value
     */
    public static void setProperty(XMLReader parser, String name, boolean value) {
        try {
            parser.setProperty(name, value);
        } catch (SAXException e) {
        	Status warningStatus = new Status(
        			Status.ERROR, CommonPlugin.PLUGIN_ID, 0, 
        			"warning: Parser does not support feature ("+name+")", e);
        	CommonPlugin.getInstance().getLog().log(warningStatus);
        }
    }    
    
    /**
     * 
     * @param is
     * @return
     */
    public String[] getXMLErrors(org.xml.sax.InputSource is) {
//    	ClassLoader cc = Thread.currentThread().getContextClassLoader();
//    	Thread.currentThread().setContextClassLoader(getClass().getClassLoader());
		ErrorHandlerImpl h = new ErrorHandlerImpl();
        try {
        	XMLReader parser = createParser();
        	if(parser==null)
            	// TODO - Move to NLS bundle
        		return new String[]{ "error: Unable to instantiate parser ("+DEFAULT_SAX_PARSER_CLASS_NAME+")"};
        	parser.setErrorHandler(h);
            parser.parse(is);
        } catch (Exception e) {
        	if(h.errors.isEmpty()) 
            	// TODO - Move to NLS bundle
        		return new String[]{"Unidentified parser error:0:0"};
        } finally {
//        	Thread.currentThread().setContextClassLoader(cc);
        }
        return (String[])h.errors.toArray(new String[0]);        
    }

    /**
     * 
     * @param reader
     * @return
     */
    public String[] getXMLErrors(Reader reader) {
        try {
            org.xml.sax.InputSource inSource = new org.xml.sax.InputSource(reader);
            return getXMLErrors(inSource);
        } catch (Exception e) {
            return new String[]{e.getMessage()};
        }
    }
    
    /**
     * 
     * @return
     */
    String getCatalog() {

    	Bundle b = Platform.getBundle(CommonPlugin.PLUGIN_ID);
    	String location = Platform.getStateLocation(b).toString().replace('\\', '/');
    	if(!location.endsWith("/")) location += "/";
    	String urlString = null;
    	URL url = null;
    	try {
    		url = Platform.resolve(b.getEntry("/"));
        	urlString = url.toString();
        	if(!urlString.endsWith("/")) urlString += "/";
        	urlString += "schemas";
    	} catch (Exception e) {
    		e.printStackTrace();
    	}
    	File f1 = new File(url.getFile() + "/schemas/catalog.xml");
    	File f2 = new File(location + "schemas/catalog.xml");
    	if(f2.exists()) try {
    		return "file:///" + location + "schemas/catalog.xml";
    	} catch (Exception e) {
    		return null;
    	}
    	FileUtil.copyDir(f1.getParentFile(), f2.getParentFile(), true);
    	String text = FileUtil.readFile(f2);
    	while(text.indexOf("%install%") >= 0) {
    		int i = text.indexOf("%install%");
    		text = text.substring(0, i) + urlString + text.substring(i + 9);
    	}
    	FileUtil.writeFile(f2, text);
    	return "file:///" + location + "schemas/catalog.xml";
    }
} 

/**
 * 
 * @author eskimo(dgolovin@exadel.com)
 *
 */
class XMLCatalogResolver1 extends XMLCatalogResolver {
    
	/**
     * 
     */
	static Set literals = new HashSet();

	/**
	 * 
	 */
    public String resolveIdentifier(XMLResourceIdentifier resourceIdentifier) throws IOException, XNIException {
    	String literal = resourceIdentifier.getLiteralSystemId();
    	if(literal != null && !literals.contains(literal)) {
    		literals.add(literal);
    	}
        return super.resolveIdentifier(resourceIdentifier);
    }
    
}
