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

import java.util.StringTokenizer;

import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.loaders.XObjectLoader;
import org.jboss.tools.common.model.loaders.impl.PropertiesLoader;
import org.jboss.tools.common.model.loaders.impl.SimpleWebFileLoader;

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
	XModelObjectLoaderUtil util;
	PropertiesLoader propertiesLoader = null;

	public PositionSearcher() {}
	
	public void init(String text, XModelObject object, String attribute) {
		if(attribute != null && attribute.startsWith("&")) { //$NON-NLS-1$
			selectAttributeName = true;
			attribute = attribute.substring(1);
		}
		this.text = text;
		this.object = object;
		this.attribute = attribute;
		
		XModelObject f = object;
		while(f != null && f.getFileType() != XModelObject.FILE) f = f.getParent();
		if(f != null) {
			XObjectLoader loader = XModelObjectLoaderUtil.getObjectLoader(f);
			if(loader instanceof SimpleWebFileLoader) {
				//TODO have more common case, this does not include WebProcessLoader
				SimpleWebFileLoader fileLoader = (SimpleWebFileLoader)loader;
				fileLoader.createRootElement(f); // initializes namespaces if available.
				util = fileLoader.getUtil();
			} else if(loader instanceof PropertiesLoader) {
				propertiesLoader = (PropertiesLoader)loader;
			}
		}
		
	}
	
	public void execute() {
		startPosition = -1;
		endPosition = -1;
		if(text == null || object == null) return;
		if(propertiesLoader != null) {
			String name = object.getAttributeValue("name"); //$NON-NLS-1$
			String dname = object.getAttributeValue("dirtyname"); //$NON-NLS-1$
			String nvs = object.getAttributeValue("name-value-separator"); //$NON-NLS-1$
			int i = text.indexOf(dname + nvs);
			if(i >= 0) {
				i = text.indexOf(name, i);
				startPosition = i;
				endPosition = i + name.length();
			}
			
		} else {
			TagIterator it = new TagIterator();
			it.selectObject(object);
			if(it.startPos < 0 || it.endPos < it.startPos) return;
			startPosition = it.startPos;
			endPosition = it.endPos;
///		findTagEnd();
			selectAttribute();
		}
	}
	
	private void selectAttribute() { 
		if(attribute == null || attribute.length() == 0) return;
		XAttribute a = object.getModelEntity().getAttribute(attribute);
		if(isESB(a)) {
			findESBAttrPosition(a);
		}
		String xml = (a == null) ? null : a.getXMLName();
		if(xml == null || xml.length() == 0) return;
		if(xml.indexOf(".") < 0) { //$NON-NLS-1$
			String s = text.substring(startPosition, endPosition);
			int i1 = findAttrPosition(s, xml);
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
			int e1 = text.indexOf("</" + getTagXMLName(object) + ">", startPosition); //$NON-NLS-1$ //$NON-NLS-2$
			String s = e1 < 0 ? "" : text.substring(startPosition, e1); //$NON-NLS-1$
			if(xml.length() == 0) {
				int i1 = s.indexOf(">"); //$NON-NLS-1$
				endPosition = startPosition + i1 + 1;
				startPosition = startPosition + e1;
			} else if(s.length() > 0) {
				int i1a = s.indexOf("<" + xml + ">"); //$NON-NLS-1$ //$NON-NLS-2$
				int i1b = s.indexOf("<" + xml + "/>"); //$NON-NLS-1$ //$NON-NLS-2$
				int i2 = (i1a < 0) ? -1 : s.indexOf("</", i1a); //$NON-NLS-1$
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

	private String getTagXMLName(XModelObject object) {
		String TAG_ATTR = "tag";//$NON-NLS-1$
		String result = null;
		if(object.getModelEntity().getAttribute(TAG_ATTR) != null) {
			result = object.getAttributeValue(TAG_ATTR);
		} else {
			result = object.getModelEntity().getXMLSubPath();
			if(result != null && util != null) {
				result = util.applyNamespaceToTag(result);
			}
		}
		return result;
	}

	private int findAttrPosition(String s, String name) {
		int i = s.indexOf(name);
		if(i < 0) {
			return -1;
		}
		StringTokenizer st = new StringTokenizer(s, "\"", true); //$NON-NLS-1$
		int pos = 0;
		boolean inValue = false;
		while(st.hasMoreTokens()) {
			String t = st.nextToken();
			if(t.equals("\"")) { //$NON-NLS-1$
				inValue = !inValue;
			} else {
				if(!inValue) {
					int k = t.indexOf(name);
					if(k >= 0) {
						boolean ok = true;
						if(k > 0) {
							char c = t.charAt(k - 1);
							if(Character.isJavaIdentifierPart(c) || c == '-') ok = false;
						}
						if(ok) return pos + k;
					}
				}
			}
			pos += t.length();
		}
		return -1;
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
		String tagname = getTagXMLName(object);
		if(tagname == null || tagname.length() == 0) return;
		String start = "<" + tagname; //$NON-NLS-1$
		if(text.indexOf(start, startPosition) != startPosition) return;
		String finish = "</" + tagname + ">"; //$NON-NLS-1$ //$NON-NLS-2$
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
			String xml = getTagXMLName(object);
			String token = "<" + xml; //$NON-NLS-1$
			if(object.getFileType() == XModelObject.FILE) {
				startPos = nextStartPos(token, startPos + 1);
				if(startPos >= 0) {
					endPos = text.indexOf(">", startPos + 1); //$NON-NLS-1$
					if(endPos < 0) endPos = text.indexOf("<", startPos + 1); //$NON-NLS-1$
					if(endPos < 0) endPos = text.length(); else endPos++;
				}
			} else if(token.equals("<")) { //$NON-NLS-1$
				selectObject(object.getParent());
			} else {
				XModelObject parent = object.getParent();
				selectObject(parent);
				if(startPos < 0) return;
//				String entity = object.getModelEntity().getName();
				XModelObject[] os = parent.getChildren();
				for (int i = 0; i < os.length; i++) {
					String xml_i = getTagXMLName(os[i]);
					if(!xml.equals(xml_i)) continue;
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
						endPos = text.indexOf(">", startPos + 1); //$NON-NLS-1$
						if(endPos < 0) endPos = text.indexOf("<", startPos + 1); //$NON-NLS-1$
						if(endPos < 0) endPos = text.length(); else endPos++;
						return;
					}					 
				}
			}			
		}

		int nextStartPos(String token, int b) {
			int s = text.indexOf(token, b);
			if(s < 0) return s;
			int cb = text.indexOf("<!--", b); //$NON-NLS-1$
			if(cb < 0 || cb > s) return s;
			int ce = text.indexOf("-->", cb); //$NON-NLS-1$
			if(ce < 0) return -1;
			return nextStartPos(token, ce + 3);
		}

	}

	/**
	 * Temporal solution; should be refactored to a framework.
	 * 
	 */
	boolean isESB(XAttribute a) {
		return a != null && "true".equals(a.getProperty("pre"));
	}

	void findESBAttrPosition(XAttribute a) {
		int ep = text.indexOf("</action>", startPosition);
		if(ep < 0) return;
		
		String dt = text.substring(startPosition, ep);
		
		String name = "name=\"" + a.getXMLName() + "\"";
		int name_i = dt.indexOf(name);
		if(name_i < 0) return;
		int ps = dt.lastIndexOf("<property", name_i);
		if(ps < 0) return;
		int pe = dt.indexOf("/>", ps);
		if(pe < 0) return;
		String dt2 = dt.substring(ps, pe + 2);
		int value_i = dt2.indexOf("value=");
		
		if(value_i >= 0) {
			int i2 = dt2.indexOf('"', value_i);
			int i3 = (i2 < 0) ? -1 : dt2.indexOf('"', i2 + 1);
			if(i3 > i2 + 1) {
				startPosition = startPosition + ps + i2 + 1;
				endPosition = startPosition + i3 - i2 - 1;
				return;
			} else if(i3 == i2 + 1) {
				startPosition = startPosition + ps + i2;
				endPosition = startPosition + 2;
				return;
			}
		}
		startPosition = startPosition + ps;
		endPosition = startPosition + pe + 2 - ps;
	}
	
}
