/******************************************************************************* 
 * Copyright (c) 2013 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.text.ext.test;

import java.util.HashMap;

import junit.framework.TestCase;

import org.eclipse.wst.html.core.internal.document.DOMStyleModelImpl;
import org.eclipse.wst.sse.core.internal.provisional.IndexedRegion;
import org.eclipse.wst.xml.core.internal.provisional.IXMLNamespace;
import org.jboss.tools.common.text.ext.util.Utils;
import org.jboss.tools.common.text.ext.util.Utils.AttrNodePair;
import org.w3c.dom.Document;
import org.w3c.dom.DocumentType;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

public class UtilsTest extends TestCase {
//	<root attr="value">
//		<node1 attr1="value1">
//			textValue
//		</node1>
//		<node2 />
//		<node3 attr3="value3" />
//		<node4 />
//	</root>
	

	private Document document;
	
	// key - node name, value - node position
	private HashMap<String, Integer> positions = new HashMap<String, Integer>();
	
	
	@Override
	protected void setUp() throws Exception {
		if(document == null){
			document = loadXML();
		}
	}
	
	public void testRoot(){
		checkNode("root"); //$NON-NLS-1$
	}
	
	public void testAttr(){
		checkAttribute("root", "attr"); //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	public void testNode1(){
		checkNode("node1"); //$NON-NLS-1$
	}
	
	public void testAttr1(){
		checkAttribute("node1", "attr1"); //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	public void testNode2(){
		checkNode("node2"); //$NON-NLS-1$
	}

	public void testNode3(){
		checkNode("node3"); //$NON-NLS-1$
	}
	
	public void testAttr3(){
		checkAttribute("node3", "attr3"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public void testNode4(){
		checkNode("node4"); //$NON-NLS-1$
	}

	private void checkNode(String nodeName){
		int position = positions.get(nodeName);
		
		AttrNodePair pair = Utils.findAttrNodePairForOffset(document, position);
		
		assertNotNull("Pair not found", pair); //$NON-NLS-1$
		
		assertNotNull("Node not found", pair.getNode()); //$NON-NLS-1$
		
		assertNull("Attribute found", pair.getAttribute()); //$NON-NLS-1$
		
		assertEquals("Wrong node found", nodeName, pair.getNode().getNodeName()); //$NON-NLS-1$
	}
	
	private void checkAttribute(String nodeName, String attrName){
		int position = positions.get(attrName);
		
		AttrNodePair pair = Utils.findAttrNodePairForOffset(document, position);
		
		assertNotNull("Pair not found", pair); //$NON-NLS-1$
		
		assertNotNull("Node not found", pair.getNode()); //$NON-NLS-1$
		
		assertNotNull("Attribute not found", pair.getAttribute()); //$NON-NLS-1$
		
		assertEquals("Wrong node found", nodeName, pair.getNode().getNodeName()); //$NON-NLS-1$
		
		assertEquals("Wrong attribute found", attrName, pair.getAttribute().getNodeName()); //$NON-NLS-1$
	}
	
	private Document loadXML(){
		DOMStyleModelImpl model = new DOMStyleModelImpl();
		
		DocumentType type = model.createDocumentType("qualifiedName", "publicId", "systemId");
		Document document = model.createDocument(IXMLNamespace.XML_URI, "qualifiedName", type);
		
		Element root = document.createElement("root"); //$NON-NLS-1$
		root.setAttribute("attr", "value"); //$NON-NLS-1$ //$NON-NLS-2$
		
		Element node1 = document.createElement("node1"); //$NON-NLS-1$
		node1.setAttribute("attr1", "value1"); //$NON-NLS-1$ //$NON-NLS-2$
		Text text = document.createTextNode("textValue"); //$NON-NLS-1$
		node1.appendChild(text);
		root.appendChild(node1);
		
		Element node2 = document.createElement("node2"); //$NON-NLS-1$
		root.appendChild(node2);

		Element node3 = document.createElement("node3"); //$NON-NLS-1$
		node3.setAttribute("attr3", "value3"); //$NON-NLS-1$ //$NON-NLS-2$
		root.appendChild(node3);

		Element node4 = document.createElement("node4"); //$NON-NLS-1$
		root.appendChild(node4);
	
		document.appendChild(root);
		
		calculatePositions(document);
		
		return document;
	}
	
	private void calculatePositions(Node node){
		positions.put(node.getNodeName(), ((IndexedRegion)node).getStartOffset());
		NamedNodeMap attrs = node.getAttributes();
		if(attrs != null){
			for(int index = 0; index < attrs.getLength(); index++){
				Node attr = attrs.item(index);
				positions.put(attr.getNodeName(), ((IndexedRegion)attr).getStartOffset());
			}
		}
		NodeList children = node.getChildNodes();
		for(int index = 0; index < children.getLength(); index++){
			Node child = children.item(index);
			calculatePositions(child);
		}
	}
}
