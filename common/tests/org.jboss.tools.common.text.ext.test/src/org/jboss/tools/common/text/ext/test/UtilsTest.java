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

import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.util.HashMap;

import junit.framework.TestCase;

import org.eclipse.jface.text.BadLocationException;
import org.eclipse.jface.text.IRegion;
import org.eclipse.wst.html.core.internal.document.DOMStyleModelImpl;
import org.eclipse.wst.sse.core.internal.model.ModelManagerImpl;
import org.eclipse.wst.sse.core.internal.provisional.IndexedRegion;
import org.eclipse.wst.sse.core.internal.provisional.text.IStructuredDocument;
import org.jboss.tools.common.text.ext.util.Utils;
import org.jboss.tools.common.text.ext.util.Utils.AttrNodePair;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.w3c.dom.Text;

public class UtilsTest extends TestCase {
	
//	<root attr0="value0">
//		<node1 attr1="value1">
//			textValue0
//		</node1>
//		<node2 />
//		<node3 attr3="value3" />
//		<node4 />
//	</root>
	

	private Document xmlDocument;
	
	private IStructuredDocument document;
	
	// key - node name, value - node position
	private HashMap<String, Integer> positions = new HashMap<String, Integer>();
	
	
	@Override
	protected void setUp() throws Exception {
		if(xmlDocument == null){
			xmlDocument = loadXML();
		}
	}
	
	public void testRoot(){
		checkNode("root"); //$NON-NLS-1$
	}
	
	public void testAttr() throws BadLocationException{
		checkAttribute("root", "attr0", "value0"); //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	public void testNode1(){
		checkNode("node1"); //$NON-NLS-1$
	}
	
	public void testAttr1() throws BadLocationException{
		checkAttribute("node1", "attr1", "value1"); //$NON-NLS-1$ //$NON-NLS-2$
	}
	
	public void testNode2(){
		checkNode("node2"); //$NON-NLS-1$
	}

	public void testNode3(){
		checkNode("node3"); //$NON-NLS-1$
	}
	
	public void testAttr3() throws BadLocationException{
		checkAttribute("node3", "attr3", "value3"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	public void testNode4(){
		checkNode("node4"); //$NON-NLS-1$
	}

	private void checkNode(String nodeName){
		int position = positions.get(nodeName);
		
		AttrNodePair pair = Utils.findAttrNodePairForOffset(xmlDocument, position);
		
		assertNotNull("Pair not found", pair); //$NON-NLS-1$
		
		assertNotNull("Node not found", pair.getNode()); //$NON-NLS-1$
		
		assertNull("Attribute found", pair.getAttribute()); //$NON-NLS-1$
		
		assertEquals("Wrong node found", nodeName, pair.getNode().getNodeName()); //$NON-NLS-1$
	}
	
	private void checkAttribute(String nodeName, String attrName, String attrValue) throws BadLocationException{
		int position = positions.get(attrName);
		
		AttrNodePair pair = Utils.findAttrNodePairForOffset(xmlDocument, position);
		
		assertNotNull("Pair not found", pair); //$NON-NLS-1$
		
		assertNotNull("Node not found", pair.getNode()); //$NON-NLS-1$
		
		assertNotNull("Attribute not found", pair.getAttribute()); //$NON-NLS-1$
		
		assertEquals("Wrong node found", nodeName, pair.getNode().getNodeName()); //$NON-NLS-1$
		
		assertEquals("Wrong attribute found", attrName, pair.getAttribute().getNodeName()); //$NON-NLS-1$
		
		IRegion region = Utils.getAttributeValueRegion(document, pair.getAttribute());
		
		assertNotNull("Attribute Value Region not found", region); //$NON-NLS-1$
		
		String documentText = document.get();
		
		int attrValuePosition = documentText.indexOf(attrValue);
		
		assertTrue("Attribute value not found", attrValuePosition > 0);
		
		assertEquals("Wrong Attribute Value Region", attrValuePosition, region.getOffset());
		
		assertEquals("Wrong Length of Attribute Value Region", attrValue.length(), region.getLength());
	}
	
	private Document loadXML() throws UnsupportedEncodingException, IOException {
		DOMStyleModelImpl model = (DOMStyleModelImpl)ModelManagerImpl.getInstance().createUnManagedStructuredModelFor("org.eclipse.wst.html.core.htmlsource");
		
		Document doc = model.getDocument();
		
		Element root = doc.createElement("root"); //$NON-NLS-1$
		root.setAttribute("attr0", "value0"); //$NON-NLS-1$ //$NON-NLS-2$
		
		Element node1 = doc.createElement("node1"); //$NON-NLS-1$
		node1.setAttribute("attr1", "value1"); //$NON-NLS-1$ //$NON-NLS-2$
		Text text = doc.createTextNode("textValue0"); //$NON-NLS-1$
		node1.appendChild(text);
		root.appendChild(node1);
		
		Element node2 = doc.createElement("node2"); //$NON-NLS-1$
		root.appendChild(node2);

		Element node3 = doc.createElement("node3"); //$NON-NLS-1$
		node3.setAttribute("attr3", "value3"); //$NON-NLS-1$ //$NON-NLS-2$
		root.appendChild(node3);

		Element node4 = doc.createElement("node4"); //$NON-NLS-1$
		root.appendChild(node4);
	
		doc.appendChild(root);
		
		calculatePositions(doc);
		
		document = model.getStructuredDocument();
		
		return doc;
	}
	
	private void calculatePositions(Node node){
		positions.put(node.getNodeName(), ((IndexedRegion)node).getStartOffset());
		if(node.hasAttributes()){
			NamedNodeMap attrs = node.getAttributes();
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
