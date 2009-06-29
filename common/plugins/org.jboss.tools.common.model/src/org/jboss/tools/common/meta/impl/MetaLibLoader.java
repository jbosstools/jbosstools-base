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
package org.jboss.tools.common.meta.impl;

import org.xml.sax.ContentHandler;
import java.io.*;
import java.util.*;
import java.net.*;

import org.w3c.dom.*;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

import org.jboss.tools.common.CommonPlugin;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.xml.SAXValidator;
import org.jboss.tools.common.xml.XMLEntityResolver;
import org.jboss.tools.common.xml.XMLEntityResolverImpl;
import org.jboss.tools.common.xml.XMLUtilities;

public class MetaLibLoader {
	public static String DOC_PUBLICID = "-//Red Hat, Inc.//DTD Meta 1.0//EN"; //$NON-NLS-1$
	
	public static boolean validateMetaXML = false;

    static {
        try {
            Class<?> c = MetaLibLoader.class;
            XMLEntityResolver.registerPublicEntity(DOC_PUBLICID, c, "/meta/meta.dtd"); //$NON-NLS-1$
        } catch (IOException e) {
        	ModelPlugin.getPluginLog().logError(e);
        }
    }
    
	
	private XModelMetaDataImpl meta = null;
    private HashSet<String> metas = new HashSet<String>();
    private ArrayList<ModuleRef> metarefs = new ArrayList<ModuleRef>();
    
    private Element root = XMLUtilities.createDocumentElement("meta"); //$NON-NLS-1$

    public MetaLibLoader() {}

    public void load(XModelMetaDataImpl meta) {
        this.meta = meta;
		Map<String,URL> resources = MetaResourceLoader.getMetaResources();
		Iterator<String> it = resources.keySet().iterator();
		while(it.hasNext()) {
			String path = it.next();
			URL url = resources.get(path);
			load(path, url);
		}

//		long t = System.currentTimeMillis();
        for (int i = 0; i < metarefs.size(); i++) {
            ModuleRef r = metarefs.get(i);
            load(r.element, r.name, r.info);
        }
//		long dt = - t + (t = System.currentTimeMillis());
//		System.out.println("Loaded from elements in " + dt + " ms");
    }

    void sift(Set modules) {
        boolean b = true;
        while(b) {
            b = false;
            for (int i = metarefs.size() - 1; i >= 0; i--) {
                ModuleRef r = metarefs.get(i);
                if(r.acceptable(modules)) continue;
                b = true;
                metarefs.remove(i);
            }
        }
    }
    
    void load(String name, URL url) {
    	if(url == null) return;
//		long t = System.currentTimeMillis();
		InputStream stream1 = null;
		InputStream stream2 = null;
		try {
			stream1 = url.openStream();
			stream2 = new BufferedInputStream(stream1, 16384);
		
		
			Element g = parse(stream2);
			if(g == null) {
				ModelPlugin.getPluginLog().logInfo("Corrupted meta resource " + name); //$NON-NLS-1$
			} else {
				load0(g, name, url.toString());
			}
		
		} catch (IOException e) {
			ModelPlugin.getPluginLog().logError("MetaLoader: Cannot read resource " + url.toString()); //$NON-NLS-1$
			return;
		} finally {
			if(stream2!=null) {
				try {
					stream2.close();
				} catch (IOException e) {
					// ignore
				}
			}
		}
		
//		long dt = - t + (t = System.currentTimeMillis());
//		System.out.println("Loaded " + url + " in " + dt + " ms");
		if(validateMetaXML) {
			InputStream stream3 = null;
			try {
				stream3 = url.openStream();
				InputSource is = new InputSource(stream3);
				String[] errors = XMLUtil.getXMLErrors(is, true);
				if(errors != null && errors.length > 0) {
					ModelPlugin.getPluginLog().logInfo("Errors in " + name); //$NON-NLS-1$
					for (int i = 0; i < errors.length && i < 5; i++) {
						ModelPlugin.getPluginLog().logInfo(errors[i]);
					}
				}
			} catch (IOException e) {
				ModelPlugin.getPluginLog().logError(e);
			}finally {
				if(stream3!=null) {
					try {
						stream3.close();
					} catch (IOException e) {
						// ignore
					}
				}
			}
//			dt = - t + (t = System.currentTimeMillis());
//			System.out.println("Validated " + url + " in " + dt + " ms");
		}
		
    }
    
    Element parse(InputStream stream) {
		Parser p = new Parser();
		p.documentElement = root;
		p.current = root;
		p.parse(stream);
		Element g = p.documentElement;
		g = XMLUtilities.getUniqueChild(g, "XModelEntityGroup"); //$NON-NLS-1$
		p.documentElement.removeChild(g);
		return g;
    }

    void load0(Element g, String name, String source) {
        metarefs.add(new ModuleRef(g, name, source));
    }

    void load(Element g, String name, String source) {
        source = source.substring(0, source.length() - name.length());
        if(metas.contains(name)) {
//            ModelPlugin.log("Can't load module " + name + " second time from " + source);
        } else if(g == null) {
            //ModelPlugin.log("Can't load module " + name + " from " + source);
        } else {
            XMetaDataLoader.loadEntityGroup(meta, g);
            metas.add(name);
            //ModelPlugin.log("Module " + name + " loaded from " + source);
            meta.getLoadedModules().put(name, source);
        }
    }

}

class ModuleRef {
    Element element;
    String name;
    String info;
    String key = ""; //$NON-NLS-1$
    ArrayList<String> depends = new ArrayList<String>();

    public ModuleRef(Element e, String name, String info) {
        this.element = e;
        Element v = XMLUtil.getUniqueChild(e, "VERSION"); //$NON-NLS-1$
        this.name = name;
        this.info = info;
        if(v == null) return;
        if(v.hasAttribute("MODULE") && v.hasAttribute("VERSION")) { //$NON-NLS-1$ //$NON-NLS-2$
            key =  v.getAttribute("MODULE") + ":" + v.getAttribute("VERSION"); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
        if(v.hasAttribute("DEPENDS")) { //$NON-NLS-1$
            String s = v.getAttribute("DEPENDS"); //$NON-NLS-1$
            StringTokenizer st = new StringTokenizer(s, ","); //$NON-NLS-1$
            while(st.hasMoreTokens()) depends.add(st.nextToken());
        }
    }

    public boolean acceptable(Set modules) {
        return (true || key.endsWith("1.0")); //$NON-NLS-1$
    }

}

class Parser implements ContentHandler {
	protected static final String FATAL_ERROR_PROCESSING_FEATURE_ID 	= "http://apache.org/xml/features/continue-after-fatal-error"; //$NON-NLS-1$
	protected static final String ENTITY_RESOLVER_PROPERTY_ID 			= "http://apache.org/xml/properties/internal/entity-resolver"; //$NON-NLS-1$
	protected static final String NAMESPACES_FEATURE_ID 				= "http://xml.org/sax/features/namespaces"; //$NON-NLS-1$
    protected static final String NAMESPACE_PREFIXES_FEATURE_ID 		= "http://xml.org/sax/features/namespace-prefixes"; //$NON-NLS-1$
    protected static final String VALIDATION_FEATURE_ID 				= "http://xml.org/sax/features/validation"; //$NON-NLS-1$
    protected static final String VALIDATION_SCHEMA_FEATURE_ID 			= "http://apache.org/xml/features/validation/schema"; //$NON-NLS-1$
    protected static final String VALIDATION_SCHEMA_CHECKING_FEATURE_ID = "http://apache.org/xml/features/validation/schema-full-checking"; //$NON-NLS-1$
    protected static final String VALIDATION_DYNAMIC_FEATURE_ID 		= "http://apache.org/xml/features/validation/dynamic"; //$NON-NLS-1$
    
    protected static final String DEFAULT_SAX_PARSER_CLASS_NAME 		= "org.apache.xerces.parsers.SAXParser"; //$NON-NLS-1$

    Element documentElement = null;
    Element current = null;

    public void parse(InputStream is) {
    	parse(new InputSource(is));
    }

    public void parse(org.xml.sax.InputSource is) {
    	XMLReader parser = createParser();
    	if(parser == null) return;
    	try {
    		parser.parse(is);
    	} catch (SAXException e) {
			ModelPlugin.getPluginLog().logError(e);
    	} catch (IOException e) {
			ModelPlugin.getPluginLog().logError(e);
    	}
	}
	
    XMLReader createParser() {
        DefaultHandler handler = new DefaultHandler();
        XMLReader parserInstance = null;

        try {
            parserInstance = XMLReaderFactory.createXMLReader(DEFAULT_SAX_PARSER_CLASS_NAME);
        } catch (SAXException e) {
        	return null;
        }

        SAXValidator.setFeature(parserInstance, NAMESPACES_FEATURE_ID, false);
        SAXValidator.setFeature(parserInstance, NAMESPACE_PREFIXES_FEATURE_ID, false);
        SAXValidator.setFeature(parserInstance, VALIDATION_FEATURE_ID, false);
        SAXValidator.setFeature(parserInstance, VALIDATION_SCHEMA_FEATURE_ID, false);
        SAXValidator.setFeature(parserInstance, VALIDATION_SCHEMA_CHECKING_FEATURE_ID, false);
        SAXValidator.setFeature(parserInstance, VALIDATION_DYNAMIC_FEATURE_ID, false);
        SAXValidator.setFeature(parserInstance, FATAL_ERROR_PROCESSING_FEATURE_ID, false);

        try {
            parserInstance.setProperty(ENTITY_RESOLVER_PROPERTY_ID, new XMLEntityResolverImpl());
        } catch (SAXException e1) {
        	CommonPlugin.getPluginLog().logError( e1.getMessage()+"", e1); //$NON-NLS-1$
        }
        
        parserInstance.setContentHandler(handler);
        parserInstance.setErrorHandler(handler);
        parserInstance.setContentHandler(this);
        return parserInstance;
    }
    
    //SAXParser

	public void characters(char[] ch, int start, int length) throws SAXException {
	}

	public void endDocument() throws SAXException {
	}

	public void endElement(String uri, String localName, String name) throws SAXException {
		Node p = current.getParentNode();
		if(p instanceof Element) {
			current = (Element)p;
		}
	}

	public void endPrefixMapping(String prefix) throws SAXException {}

	public void ignorableWhitespace(char[] ch, int start, int length) throws SAXException {}

	public void processingInstruction(String target, String data) throws SAXException {
	}

	public void setDocumentLocator(Locator locator) {}

	public void skippedEntity(String name) throws SAXException {}

	public void startDocument() throws SAXException {
	}

	public void startElement(String uri, String localName, String name,
			Attributes atts) throws SAXException {
		if(current == null) {
			current = XMLUtilities.createDocumentElement(name);
			documentElement = current;
		} else {
			current = XMLUtilities.createElement(current, name);
		}
		int l = atts.getLength();
		for (int i = 0; i < l; i++) {
			String n = atts.getQName(i);
			String v = atts.getValue(i);
			current.setAttribute(n, v);
		}
	}

	public void startPrefixMapping(String prefix, String uri) throws SAXException {}

}
