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

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.model.XModelObject;

/**
 * Searches for position of start tag corresponding to model object.
 * If, optionally, attribute name is provided, searches for position
 * of its value.
 * This feature is necessary because model object does not keep its 
 * position in XML text, being loaded using standard XML parser
 * that does not provide location in text.
 * The algorithm used here is not 100% safe, and should not be 
 * used for context changes, rather it can be used for 
 * synchronizing text selection to tree or diagram selection. 
 * 
 * 
 * @author glory
 */
public class PositionSearcher {
	String text;
	XModelObject object;
	String attribute;
	int startPosition;
	int endPosition;
	boolean selectAttributeName = false;

	public PositionSearcher() {}
	
	public void init(String text, XModelObject object, String attribute) {
		if(attribute != null && attribute.startsWith("&")) {
			selectAttributeName = true;
			attribute = attribute.substring(1);
		}
		this.text = text;
		this.object = object;
		this.attribute = attribute;
	}
	
	public void execute() {
		startPosition = -1;
		endPosition = -1;
		if(text == null || object == null) return;
		TagIterator it = new TagIterator();
		it.selectObject(object);
		if(it.startPos < 0 || it.endPos < it.startPos) return;
		startPosition = it.startPos;
		endPosition = it.endPos;
///		findTagEnd();
		selectAttribute();
	}
	
	private void selectAttribute() { 
		if(attribute == null || attribute.length() == 0) return;
		XAttribute a = object.getModelEntity().getAttribute(attribute);
		String xml = (a == null) ? null : a.getXMLName();
		if(xml == null || xml.length() == 0) return;
		if(xml.indexOf(".") < 0) {
			String s = text.substring(startPosition, endPosition);
			int i1 = s.indexOf(xml);
			if(selectAttributeName) {
				if(i1 < 0) return;
				startPosition = startPosition + i1;
				endPosition = startPosition + xml.length();
				return;
			}
			int i2 = (i1 < 0) ? -1 : s.indexOf('"', i1 + 1);
			int i3 = (i2 < 0) ? -1 : s.indexOf('"', i2 + 1);
			if(i3 > 0) {
				endPosition = startPosition + i3;						
				startPosition = startPosition + i2 + 1;
			}
		} else {
			xml = xml.substring(0, xml.indexOf('.'));
			int e1 = text.indexOf("</" + object.getModelEntity().getXMLSubPath() + ">", startPosition);
			String s = e1 < 0 ? "" : text.substring(startPosition, e1);
			if(xml.length() == 0) {
				int i1 = s.indexOf(">");
				endPosition = startPosition + i1 + 1;
				startPosition = startPosition + e1;
			} else if(s.length() > 0) {
				int i1a = s.indexOf("<" + xml + ">");
				int i1b = s.indexOf("<" + xml + "/>");
				int i2 = (i1a < 0) ? -1 : s.indexOf("</", i1a);
				if(i1a >= 0 && i2 >= 0) {
					endPosition = startPosition + i2;
					startPosition = startPosition + i1a + 2 + xml.length();
				} else if(i1b >= 0) {
					endPosition = startPosition + i1b + 3 + xml.length();
					startPosition = startPosition + i1b;
				}
			}
		}				
	}
	
	public int getStartPosition() {
		return startPosition;
	}
	
	public int getEndPosition() {
		return endPosition;
	}
	
	void findTagEnd() {
		if(startPosition < 0) return;
		if(attribute != null && attribute.length() > 0) return;
		String tagname = object.getModelEntity().getXMLSubPath();
		if(tagname == null || tagname.length() == 0) return;
		String start = "<" + tagname;
		if(text.indexOf(start, startPosition) != startPosition) return;
		String finish = "</" + tagname + ">";
		int i = text.indexOf(finish, startPosition);
		if(i >= 0) endPosition = i + finish.length();
	}
	
	class TagIterator {
		int startPos;
		int endPos;
		
		TagIterator() {
			startPos = -1;
			endPos = 0;
		}
		
		public void selectObject(XModelObject object) {
			if(object == null) return;
			String xml = object.getModelEntity().getXMLSubPath();
			String token = "<" + xml;
			if(object.getFileType() == XModelObject.FILE) {
				startPos = nextStartPos(token, startPos + 1);
				if(startPos >= 0) {
					endPos = text.indexOf(">", startPos + 1);
					if(endPos < 0) endPos = text.indexOf("<", startPos + 1);
					if(endPos < 0) endPos = text.length(); else endPos++;
				}
			} else if(token.equals("<")) {
				selectObject(object.getParent());
			} else {
				XModelObject parent = object.getParent();
				selectObject(parent);
				if(startPos < 0) return;
				XModelObject[] os = parent.getChildren(object.getModelEntity().getName());
				for (int i = 0; i < os.length; i++) {
					boolean ok = false;
					while(!ok) {
						startPos = nextStartPos(token, startPos + 1);
						if(startPos < 0) return;
						if(startPos + token.length() == text.length()) {
							ok = true;
						} else {
							char ch = text.charAt(startPos + token.length());
							ok = Character.isWhitespace(ch) || ch == '/' || ch == '>';
						}						
					}
					if(os[i] == object) {
						endPos = text.indexOf(">", startPos + 1);
						if(endPos < 0) endPos = text.indexOf("<", startPos + 1);
						if(endPos < 0) endPos = text.length(); else endPos++;
						return;
					}					 
				}
			}			
		}

		int nextStartPos(String token, int b) {
			int s = text.indexOf(token, b);
			if(s < 0) return s;
			int cb = text.indexOf("<!--", b);
			if(cb < 0 || cb > s) return s;
			int ce = text.indexOf("-->", cb);
			if(ce < 0) return -1;
			return nextStartPos(token, ce + 3);
		}

	}
	
}
