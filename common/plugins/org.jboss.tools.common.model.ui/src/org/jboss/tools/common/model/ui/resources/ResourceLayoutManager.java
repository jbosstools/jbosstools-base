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
package org.jboss.tools.common.model.ui.resources;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.ParserConfigurationException;

import org.apache.xml.serialize.LineSeparator;
import org.apache.xml.serialize.Method;
import org.apache.xml.serialize.OutputFormat;
import org.apache.xml.serialize.XMLSerializer;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.core.runtime.Status;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import org.jboss.tools.common.xml.SafeDocumentBuilderFactory;
import org.jboss.tools.common.model.ui.ModelUIPlugin;
import org.jboss.tools.common.model.ui.ModelUIMessages;

/**
 * @author AU
 */
public class ResourceLayoutManager {
	
	// public	
	public static final String ROOT_TAG = "Layout";
	public static final String RESOURCE_ATTR = "resource";
	public static final String LAYOUT_EXT = "l4t";
	public static final String LAYOUT_PROPERTY = "layout";

	// static
	private static ResourceLayoutManager instance;
	private static final String ERROR_UNKNOW_EXCEPTION = "org.jboss.tools.common.model.ui.resources.ResourceLayoutManager.ERROR_UNKNOW_EXCEPTION";
	private static final String ERROR_RESOURCE_NULL = "org.jboss.tools.common.model.ui.resources.ResourceLayoutManager.ERROR_RESOURCE_NULL";
	private static final String ERROR_CREATE_DOCUMENT = "org.jboss.tools.common.model.ui.resources.ResourceLayoutManager.ERROR_CREATE_DOCUMENT";
	private static final String WARNING_EMPTY_HEAD = "org.jboss.tools.common.model.ui.resources.ResourceLayoutManager.WARNING_HEAD_EMPTY";
	private static final String ERROR_ANOTHER_HEAD = "org.jboss.tools.common.model.ui.resources.ResourceLayoutManager.ERROR_ANOTHER_HEAD";
	
	// documents hash, key == fullPathToResource, value == document
	private HashMap<String,Document> hashMap = new HashMap<String,Document>(); 

	// AnotherResourceException class
	class AnotherResourceException extends Exception {
		private static final long serialVersionUID = 1L;
		
	}

	private ResourceLayoutManager() {}
	
	public static ResourceLayoutManager getDefault() {
		if (instance==null) instance = new ResourceLayoutManager();
		return instance;
	}
	
	// return first element with specify elementTag or create new
	public Element getLayoutElement(IResource resource, String elementTag) {
		Element element = null;
		Document document = getLayoutDocument(resource);
		if(document == null) return null;
		NodeList nodeList = document.getElementsByTagName(elementTag);
		if (nodeList==null || nodeList.getLength()==0 || nodeList.item(0)==null) {
			// create new element
			element = document.createElement(elementTag);
			Node root = document.getFirstChild();
			root.appendChild(element);
		} else {
			element = (Element)nodeList.item(0); 
		}
		return element;
	}
	
	private Document getLayoutDocument(IResource resource) {
		Document document = null;
		if (resource==null) {
			ModelUIPlugin.getPluginLog().logError(ModelUIMessages.getString(ERROR_RESOURCE_NULL));
			return null;
		}
		
		String fullResourceLocation = getFullLocation(resource).toString();
		String layoutLocation = null;
		try {
			layoutLocation = resource.getPersistentProperty(new QualifiedName("",LAYOUT_PROPERTY));
		} catch (CoreException e) {}
		if (layoutLocation==null) layoutLocation = createNewLayoutLocation(resource);
		//String fullLayoutLocation = getFullLocation(resource).addFileExtension(LAYOUT_EXT).toString();
		
		// try get from hashMap
		document = (Document)hashMap.get(fullResourceLocation);
		if (document!=null) return document;

		// try load from file system
		try {
			document = loadDocument(resource);
		} catch (ParserConfigurationException e) {
			ModelUIPlugin.getPluginLog().logError(ModelUIMessages.getString(ERROR_UNKNOW_EXCEPTION), e);
		} catch (SAXException e) {
			ModelUIPlugin.getPluginLog().logError(ModelUIMessages.getString(ERROR_UNKNOW_EXCEPTION), e);
		} catch (IOException e) {
			ModelUIPlugin.getPluginLog().logError(ModelUIMessages.getString(ERROR_UNKNOW_EXCEPTION), e);
		} catch (AnotherResourceException e) {
			ModelUIPlugin.getPluginLog().logError(ModelUIMessages.getString(ERROR_ANOTHER_HEAD), e);
		}
		if (document!=null){
			hashMap.put(fullResourceLocation, document);
			return document;
		}

		// try create new document
		try {
			document = createNewDocument(resource);
		} catch (ParserConfigurationException e) {
			//log(Status.ERROR, ModelUIMessages.getString(ERROR_CREATE_DOCUMENT, new String[] {fullLayoutLocation, fullResourceLocation}));
			ModelUIPlugin.getPluginLog().logError( ModelUIMessages.getString(ERROR_CREATE_DOCUMENT, new String[] {layoutLocation, fullResourceLocation}));
		}
		if (document!=null){
			hashMap.put(fullResourceLocation, document);
			return document;
		}
		
		return document;
	}

	private Document createNewDocument(IResource resource) throws ParserConfigurationException {
		Document document = null;
		// try create DocumentBuilder
		DocumentBuilder builder = null;
		//DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
		builder = DocumentBuilderHolder.getInstance();
		// create new document
		document = builder.newDocument();
		createRoot(resource, document);
		return document;
	}
	
	private Document loadDocument(IResource resource) throws AnotherResourceException, ParserConfigurationException, SAXException, IOException {
//		String fullResourceLocation = 
			getFullLocation(resource).toString();
		String layoutLocation = null;
		try {
			layoutLocation = resource.getPersistentProperty(new QualifiedName("",LAYOUT_PROPERTY));
		} catch (CoreException e) {}
		if (layoutLocation==null) layoutLocation = createNewLayoutLocation(resource);
		if(!new File(layoutLocation).isFile()) return null;
		//String fullLayoutLocation = getFullLocation(resource).addFileExtension(LAYOUT_EXT).toString();
		Document document = null;
		DocumentBuilder builder = null;
		// try create DocumentBuilder
		builder = DocumentBuilderHolder.getInstance();
		// try parse
		//document = builder.parse(fullLayoutLocation);
		document = builder.parse(layoutLocation);
		// verify head
		if (document != null) {
			NodeList nodeList = document.getElementsByTagName(ROOT_TAG);
			if (nodeList==null || nodeList.getLength()==0 || nodeList.item(0)==null) {
				// warning
				//log(ModelUIMessages.getString(WARNING_EMPTY_HEAD, new String[] {fullLayoutLocation}));
				ModelUIPlugin.getPluginLog().logInfo(ModelUIMessages.getString(WARNING_EMPTY_HEAD, new String[] {layoutLocation}));
				// create new head
				createRoot(resource, document);
			} /*else {
				(Element)nodeList.item(0);
				String fromHead = head.getAttribute(RESOURCE_ATTR);
				String fromResource = resource.getFullPath().toString();
				if (!fromResource.equals(fromHead)) {
					throw new AnotherResourceException();
				}
			}
			*/
		}
		return document;
	}

	private IPath getFullLocation(IResource resource) {
		IProject project = resource.getProject();
		IPath layoutPath = resource.getProjectRelativePath();
		IFile layoutFile = project.getFile(layoutPath);
		return layoutFile.getLocation();
	}

	public void store(IResource resource) {
		String layoutLocation = null;
		try {
			layoutLocation = resource.getPersistentProperty(new QualifiedName("",LAYOUT_PROPERTY));
		} catch (CoreException e) {}
		if (layoutLocation==null) {
			layoutLocation = createNewLayoutLocation(resource);
		}
		if (layoutLocation==null) return;
		File file = new File(layoutLocation);
		if (!file.exists()) {
			layoutLocation = createNewLayoutLocation(resource);
		}
		String fullResourceLocation = getFullLocation(resource).toString();
		//String fullLayoutLocation = getFullLocation(resource).addFileExtension(LAYOUT_EXT).toString();
		Document document = (Document)hashMap.get(fullResourceLocation);
		if (document==null) return;
		try {
			//FileWriter writer = new FileWriter(fullLayoutLocation);
			FileWriter writer = new FileWriter(layoutLocation);
			XMLSerializer serial = new XMLSerializer(writer, createOutputFormat("UTF-8"));
			serial.asDOMSerializer();
			serial.serialize(document);
			writer.close();
		} catch (Exception e) {
			//log(Status.ERROR, ModelUIMessages.getString(ERROR_CREATE_DOCUMENT, new String[] {fullLayoutLocation, fullResourceLocation}), e);
			ModelUIPlugin.getPluginLog().logError(ModelUIMessages.getString(ERROR_CREATE_DOCUMENT, new String[] {layoutLocation, fullResourceLocation}), e);
		}
	}

	private OutputFormat createOutputFormat(String encoding) {
		if(encoding == null || encoding.length() == 0) encoding = "UTF-8";
		OutputFormat format = new OutputFormat(Method.XML, encoding, true);
		format.setLineSeparator(System.getProperty("line.separator", LineSeparator.Web));
		format.setIndent(1);
		return format;
	}
	
	private Element createRoot(IResource resource, Document document) {
		Element element = document.createElement(ROOT_TAG);
		Node oldRoot = document.getFirstChild();
		element.setAttribute(RESOURCE_ATTR, resource.getFullPath().toString());
		if (oldRoot!=null) {
			document.removeChild(oldRoot);
		}
		document.appendChild(element);
		return element;
	}
	
	
	private IPath getPluginMetadataPath() {
		return Platform.getStateLocation(Platform.getBundle(ModelUIPlugin.PLUGIN_ID));
	}
	
	private String createNewLayoutLocation(IResource resource) {
		String fileName = resource.getProject().getName()+"_"+resource.getName()+"_"+System.currentTimeMillis();
		String layoutLocation = getPluginMetadataPath().toString();
		String result = layoutLocation+"/"+fileName+"."+LAYOUT_EXT; 
		try {
			resource.setPersistentProperty(new QualifiedName("", LAYOUT_PROPERTY), result);
		} catch (CoreException e) {}
		return result; 
	}
	
	static class DocumentBuilderHolder {
		static private DocumentBuilder INSTANCE = SafeDocumentBuilderFactory.createDocumentBuilder(false);
		
		public static DocumentBuilder getInstance() {
			return INSTANCE;
		}
	}
}
