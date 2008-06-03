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
package org.jboss.tools.common.model.util;

import java.util.*;
import org.w3c.dom.*;

public class XMLMapper {
	Map<Node, int[]> mapping = new HashMap<Node, int[]>();
	String text;
	
	public XMLMapper(Document document, String text) {
		this.text = text;
		process(document);
	}
	
	int process(Node node, int index) {
		int type = node.getNodeType();
		switch (type) {
			case Node.TEXT_NODE : return processText((Text)node, index);
			case Node.CDATA_SECTION_NODE : return processCDATA((CharacterData)node, index);
			case Node.ELEMENT_NODE : return processElement((Element)node, index);
			case Node.ATTRIBUTE_NODE : return processAttr((Attr)node, index);
			case Node.COMMENT_NODE : return processComment((Comment)node, index);
		}
		return index;
	}
	
	int process(Document document) {
		int index = 0;
		NodeList n = document.getChildNodes();
		for (int i = 0; i < n.getLength(); i++) {
			Node node = n.item(i);
			index = process(node, index);
		}
		return index;
	}
	
	int processElement(Element node, int index) {
		NodeList n = node.getChildNodes();
		int start = text.indexOf("<" + node.getNodeName(), index);
		
		NamedNodeMap as = node.getAttributes();
		if(as != null) for (int i = 0; i < as.getLength(); i++) {
			Attr a = (Attr)as.item(i);
			String name = a.getNodeName();
			int ns = text.indexOf(name, start);
			if(ns < 0) continue;
			int q1 = text.indexOf('"', ns);
			int q2 = text.indexOf('"', q1 + 1);
			mapping.put(a, new int[]{ns, q2 + 1 - ns});			
			put(node, ns, q2 + 1 - ns);
		}
		
		int headerEnd = text.indexOf('>', start);
		if(text.charAt(headerEnd - 1) == '/') {
			put(node, start, headerEnd + 1);
			return headerEnd + 1;
		}
		int ciindex = headerEnd + 1;
		for (int i = 0; i < n.getLength(); i++) {
			Node cn = n.item(i);
			ciindex = process(cn, ciindex);
		}
		String token = "</" + node.getNodeName() + ">";
		int end = text.indexOf(token, ciindex) + token.length();
		put(node, start, end);
		
		return end;
	}

	int processText(Text node, int index) {
		int start = index;
		int end = text.indexOf('<', index);
		put(node, start, end);
		return end;
	}

	int processCDATA(CharacterData node, int index) {
		int start = text.indexOf("<![CDATA[", index);
		int end = text.indexOf("]]>", index) + 3;
		put(node, start, end);
		return end;
	}
	
	int processAttr(Attr node, int index) {
		
		return index;
	}

	int processComment(Comment node, int index) {
		int start = text.indexOf("<!--", index);
		int end = text.indexOf("-->", index) + 3;
		put(node, start, end);
		return end;
	}
	
	void put(Node node, int start, int end) {
		mapping.put(node, new int[]{start, end - start});
	}


}
