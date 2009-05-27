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
import java.io.IOException;
import java.io.Reader;
import java.net.URL;
import java.text.MessageFormat;
import java.util.HashSet;
import java.util.Set;

import org.apache.xerces.util.XMLCatalogResolver;
import org.apache.xerces.xni.XMLResourceIdentifier;
import org.eclipse.core.runtime.Platform;
import org.jboss.tools.common.CommonPlugin;
import org.jboss.tools.common.Messages;
import org.jboss.tools.common.util.FileUtil;
import org.osgi.framework.Bundle;
import org.xml.sax.SAXException;
import org.xml.sax.SAXNotRecognizedException;
import org.xml.sax.SAXNotSupportedException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

/**
 * @author eskimo(dgolovin@exadel.com)
 * @version   $Revision:$
 */
public class SAXValidator {
	
	protected static final String FATAL_ERROR_PROCESSING_FEATURE_ID 	= "http://apache.org/xml/features/continue-after-fatal-error"; //$NON-NLS-1$
	protected static final String ENTITY_RESOLVER_PROPERTY_ID 			= "http://apache.org/xml/properties/internal/entity-resolver"; //$NON-NLS-1$
	protected static final String NAMESPACES_FEATURE_ID 				= "http://xml.org/sax/features/namespaces"; //$NON-NLS-1$
    protected static final String NAMESPACE_PREFIXES_FEATURE_ID 		= "http://xml.org/sax/features/namespace-prefixes"; //$NON-NLS-1$
    protected static final String VALIDATION_FEATURE_ID 				= "http://xml.org/sax/features/validation"; //$NON-NLS-1$
    protected static final String VALIDATION_SCHEMA_FEATURE_ID 			= "http://apache.org/xml/features/validation/schema"; //$NON-NLS-1$
    protected static final String VALIDATION_SCHEMA_CHECKING_FEATURE_ID = "http://apache.org/xml/features/validation/schema-full-checking"; //$NON-NLS-1$
    protected static final String VALIDATION_DYNAMIC_FEATURE_ID 		= "http://apache.org/xml/features/validation/dynamic"; //$NON-NLS-1$
    
    protected static final String DEFAULT_SAX_PARSER_CLASS_NAME 		= "org.apache.xerces.parsers.SAXParser"; //$NON-NLS-1$

    /**
     * 
     * @return
     */
    XMLReader createParser() {
        DefaultHandler handler = new DefaultHandler();
        XMLReader parserInstance = null;

        try {
            parserInstance = XMLReaderFactory.createXMLReader(DEFAULT_SAX_PARSER_CLASS_NAME);
        } catch (SAXException e) {
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
        } catch (SAXNotRecognizedException e1) {
        	CommonPlugin.getPluginLog().logError( e1.getMessage()+"", e1); //$NON-NLS-1$
        } catch (SAXNotSupportedException e1) {
        	CommonPlugin.getPluginLog().logError( e1.getMessage()+"", e1); //$NON-NLS-1$
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
        	CommonPlugin.getPluginLog().logError("warning: Parser does not support feature ("+name+")", e); //$NON-NLS-1$ //$NON-NLS-2$
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
        	CommonPlugin.getPluginLog().logError("warning: Parser does not support feature ("+name+")", e); //$NON-NLS-1$ //$NON-NLS-2$
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
        	if(parser==null) {
        		return new String[]{ MessageFormat.format(
						Messages.SAXValidator_UnableToInstantiateMessage, DEFAULT_SAX_PARSER_CLASS_NAME)};
			}
        	parser.setErrorHandler(h);
            parser.parse(is);
        } catch (SAXException e) {
        	if(h.errors.isEmpty()) {
        		return new String[]{Messages.SAXValidator_SAXExceptionMessage+":0:0",e.getMessage()}; //$NON-NLS-1$
        	}
        } catch (IOException e) {
        	if(h.errors.isEmpty()) {
	        	return new String[]{Messages.SAXValidator_IOExceptionMessage+":0:0",e.getMessage()}; //$NON-NLS-1$
        	}
		} finally {
//        	Thread.currentThread().setContextClassLoader(cc);
        }
        return h.errors.toArray(new String[0]);        
    }

    /**
     * 
     * @param reader
     * @return
     */
    public String[] getXMLErrors(Reader reader) {
            org.xml.sax.InputSource inSource = new org.xml.sax.InputSource(reader);
            return getXMLErrors(inSource);
    }
    
    /**
     * 
     * @return
     */
    String getCatalog() {

    	Bundle b = Platform.getBundle(CommonPlugin.PLUGIN_ID);
    	String location = Platform.getStateLocation(b).toString().replace('\\', '/');
    	if(!location.endsWith("/")) { //$NON-NLS-1$
			location += "/"; //$NON-NLS-1$
		}
    	String urlString = null;
    	URL url = null;
    	try {
    		url = Platform.resolve(b.getEntry("/")); //$NON-NLS-1$
        	urlString = url.toString();
        	if(!urlString.endsWith("/")) { //$NON-NLS-1$
				urlString += "/"; //$NON-NLS-1$
			}
        	urlString += "schemas"; //$NON-NLS-1$
    	} catch (IOException e) {
    		CommonPlugin.getPluginLog().logError(e);
    	}
    	File f1 = new File(url.getFile() + "/schemas/catalog.xml"); //$NON-NLS-1$
    	File f2 = new File(location + "schemas/catalog.xml"); //$NON-NLS-1$
    	if(f2.exists()) {
    		return "file:///" + location + "schemas/catalog.xml"; //$NON-NLS-1$ //$NON-NLS-2$
    	}
    	FileUtil.copyDir(f1.getParentFile(), f2.getParentFile(), true);
    	String text = FileUtil.readFile(f2);
    	while(text.indexOf("%install%") >= 0) { //$NON-NLS-1$
    		int i = text.indexOf("%install%"); //$NON-NLS-1$
    		text = text.substring(0, i) + urlString + text.substring(i + 9);
    	}
    	FileUtil.writeFile(f2, text);
    	return "file:///" + location + "schemas/catalog.xml"; //$NON-NLS-1$ //$NON-NLS-2$
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
    @Override
	public String resolveIdentifier(XMLResourceIdentifier resourceIdentifier) throws IOException {
    	String literal = resourceIdentifier.getLiteralSystemId();
    	if(literal != null && !literals.contains(literal)) {
    		literals.add(literal);
    	}
        return super.resolveIdentifier(resourceIdentifier);
    }
    
}
