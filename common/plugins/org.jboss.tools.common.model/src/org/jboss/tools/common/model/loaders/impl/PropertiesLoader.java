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
package org.jboss.tools.common.model.loaders.impl;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.util.*;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.loaders.*;
import org.jboss.tools.common.model.util.XModelObjectLoaderUtil;
import org.jboss.tools.common.model.event.XModelTreeEvent;
import org.jboss.tools.common.model.impl.*;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.util.FileUtil;

public class PropertiesLoader implements XObjectLoader {
	public static String ENT_PROPERTY = "Property"; //$NON-NLS-1$
	static String INTERNAL_SEPARATOR = "@"; //$NON-NLS-1$
	String defaultLineSeparator = "\r\n"; //$NON-NLS-1$

    public PropertiesLoader() {}

    public static String getEncoding(XModelObject object) {
    	if(!object.isActive()) {
    		String encoding = object.get("_encoding_"); //$NON-NLS-1$
    		return encoding != null && encoding.length() > 0 ? encoding : "8859_1"; //$NON-NLS-1$
    	}
    	IResource resource = (IResource)object.getAdapter(IResource.class);
    	if(!(resource instanceof IFile)) return null;
    	IFile f = (IFile)resource;
    	return FileUtil.getEncoding(f);
    }

    public void load(XModelObject object) {
        String encoding = getEncoding(object);

        object.setAttributeValue("encoding", encoding); //$NON-NLS-1$
        String body = XModelObjectLoaderUtil.getTempBody(object);
        EncodedProperties properties = new EncodedProperties();
        properties.setEncoding(encoding);
        Properties mapping = new Properties();
        try {
        	ByteArrayInputStream s = new ByteArrayInputStream(body.getBytes(encoding));
			properties.load(s);
			Iterator it = properties.keySet().iterator();
			while(it.hasNext()) {
				String nm = it.next().toString();
				String sn = EncodedProperties.saveConvert(nm, true); // convertName(nm);
				mapping.put(sn, nm);
			}
        } catch (IOException e) {
        	//ignore
        }

        StringTokenizer st = new StringTokenizer(body, "\n\r", true); //$NON-NLS-1$
        StringBuilder sb = new StringBuilder();
        StringBuilder lineEnd = new StringBuilder();
        int state = 0;
        XModelObject c = null;
        while(st.hasMoreTokens()) {
            String s = st.nextToken();
            if(s.equals("\r")) { //$NON-NLS-1$
				if(state != 2) sb.append(s); else lineEnd.append(s);
            	continue;
            } 
            if(s.equals("\n")) { //$NON-NLS-1$
                if(state != 2) sb.append(s); else lineEnd.append(s);
                if(state == 0) {
                	state = 1;
                } else if(state == 2) {
                	state = 0;
					c.setAttributeValue("dirtyvalue", sb.toString()); //$NON-NLS-1$
					c.setAttributeValue("line-end", lineEnd.toString()); //$NON-NLS-1$
					sb.setLength(0);
					lineEnd.setLength(0);
                } 
                continue;
            }
            lineEnd.setLength(0);
			if(state == 3) {
				if(!s.endsWith("\\")) { //$NON-NLS-1$
					sb.append(s);
					state = 2;
				} else {
					sb.append(s.substring(0, s.length() - 1)).append(INTERNAL_SEPARATOR); 
				}
				continue;
			}
			int i = getSeparatorIndex(s);
            if(i < 0) {
				sb.append(s);
				state = 1;
            	continue;
            }
			String dirtyName = s.substring(0, i);
			String name = dirtyName.trim();
			String visualName = mapping.getProperty(name);
			if(visualName == null || !properties.containsKey(visualName)) {
				sb.append(s);
				state = 1;
				continue;
			}
            String comments = sb.toString();
            sb.setLength(0);
            String value = properties.getProperty(visualName);
            Properties p = new Properties();
            p.setProperty(XModelObjectConstants.ATTR_NAME, visualName);
			p.setProperty("dirtyname", dirtyName); //$NON-NLS-1$
            p.setProperty("value", value); //$NON-NLS-1$
            
            p.setProperty("name-value-separator", i == s.length() ? " " : "" + s.charAt(i)); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
            p.setProperty("comments", comments); //$NON-NLS-1$
            p.setProperty("separator", "#"); //obsolete //$NON-NLS-1$ //$NON-NLS-2$
            p.setProperty("line-end", ""); //$NON-NLS-1$ //$NON-NLS-2$

            c = object.getModel().createModelObject(ENT_PROPERTY, p);
            object.addChild(c);

            String dirtyvalue = (i < s.length()) ? s.substring(i + 1) : ""; //$NON-NLS-1$
            if(s.endsWith("\\")) { //$NON-NLS-1$
            	state = 3;
            	sb.append(dirtyvalue.substring(0, dirtyvalue.length() - 1)).append(INTERNAL_SEPARATOR);
            } else {
            	state = 2;
				sb.append(dirtyvalue);
            }
        }
        if(state == 1 && sb.length() > 0) {
        	object.set("conclusion", sb.toString());  //$NON-NLS-1$
        }
        if(state == 2) {
			c.setAttributeValue("dirtyvalue", sb.toString()); //$NON-NLS-1$
			c.setAttributeValue("line-end", lineEnd.toString()); //$NON-NLS-1$
        }
    }

    public boolean update(XModelObject object) throws XModelException {
    	String encoding = getEncoding(object);
        XModelObject c = object.copy(0);
        if(encoding != null) c.set("_encoding_", encoding); //$NON-NLS-1$
		XModelObjectLoaderUtil.setTempBody(c, XModelObjectLoaderUtil.getTempBody(object));
        load(c);
        ////EnginesLoader.merge(object, c, false);
        merge(object, c);
        object.setModified(false);
        return true;
    }

    public boolean save(XModelObject object) {
        if(!object.isModified()) return true;
		XModelObjectLoaderUtil.setTempBody(object, generateBody(object, defaultLineSeparator));
        object.setModified(true);
        return true;
    }

    private void appendComments(StringBuffer sb, String comments, String commentSeparator, String lineSeparator) {
        if(comments.length() == 0) return;
        sb.append(comments);
    }

	public String getBody(XModelObject object) {
		return generateBody(object, defaultLineSeparator);	
	}
		
    String generateBody(XModelObject object, String lineSeparator) {
		StringBuffer sb = new StringBuffer();
		XModelObject[] cs = object.getChildren();
		for (int i = 0; i < cs.length; i++) {
			String name_value_separator = cs[i].getAttributeValue("name-value-separator"); //$NON-NLS-1$
			if(name_value_separator == null || name_value_separator.length() != 1 || " \t=:".indexOf(name_value_separator) < 0) { //$NON-NLS-1$
				name_value_separator = "="; //$NON-NLS-1$
			}
			appendComments(sb, cs[i].get("COMMENTS"), cs[i].get("SEPARATOR"), lineSeparator); //$NON-NLS-1$ //$NON-NLS-2$
			if(XModelObjectConstants.NO.equals(cs[i].get("ENABLED"))) sb.append('#'); //$NON-NLS-1$
			String dirtyname = cs[i].getAttributeValue("dirtyname"); //$NON-NLS-1$
			String name = EncodedProperties.saveConvert(cs[i].get(XModelObjectConstants.XML_ATTR_NAME), true); // convertName(cs[i].get(XModelObjectConstants.XML_ATTR_NAME));
			String value = cs[i].get("VALUE"); //$NON-NLS-1$
			String dirtyvalue = cs[i].getAttributeValue("dirtyvalue"); //$NON-NLS-1$
			if(value == null || dirtyvalue == null || !value.equals(dirtyvalue.trim())) {
				value = EncodedProperties.saveConvert(value, false); // convertValue(value);
			}
			String resolved = resolveValue(value, dirtyvalue);
			//preserve one white space after separator
			if(dirtyvalue != null && dirtyvalue.startsWith(" ")  //$NON-NLS-1$
					&& resolved != null && resolved.length() > 0 && !resolved.startsWith(" ") //$NON-NLS-1$
					&& !name_value_separator.endsWith(" ")) { //$NON-NLS-1$
				resolved = " " + resolved; //$NON-NLS-1$
			}
			if(dirtyname != null && name.equals(dirtyname.trim())) name = dirtyname;
			//preserve one white space before separator
			if(dirtyname != null && dirtyname.endsWith(" ")  //$NON-NLS-1$
					&& name != null && name.length() > 0 && !name.endsWith(" ") //$NON-NLS-1$
					&& !name_value_separator.startsWith(" ")) { //$NON-NLS-1$
				name = name + " "; //$NON-NLS-1$
			}
			sb.append(name);
			if(!" ".equals(name_value_separator) || resolved.length() > 0) { //$NON-NLS-1$
				sb.append(name_value_separator);
			}
			sb.append(resolved);
			String ls = cs[i].get("line-end"); //$NON-NLS-1$
			if(ls.length() > 0) {
				if(ls.equals("\\r\\n")) ls = defaultLineSeparator; //$NON-NLS-1$
				sb.append(ls);
			} else if(i < cs.length - 1) {
				ls = defaultLineSeparator;
				sb.append(ls);
			}
		}
		String conclusion = object.get("conclusion"); //$NON-NLS-1$
		if(conclusion != null) sb.append(conclusion);
		return sb.toString();    	
    }
    
    String resolveValue(String value, String dirtyvalue) {
    	if(dirtyvalue == null) return value;
    	if(dirtyvalue.trim().equals(value)) return dirtyvalue;
    	if(dirtyvalue.indexOf(INTERNAL_SEPARATOR) < 0) return value;
    	StringTokenizer st = new StringTokenizer(dirtyvalue, INTERNAL_SEPARATOR, true);
    	StringBuffer cv = new StringBuffer();
    	StringBuffer dv = new StringBuffer();
    	String rightWhites = ""; //$NON-NLS-1$
    	while(st.hasMoreTokens()) {
    		String t = st.nextToken();
    		if(t.equals(INTERNAL_SEPARATOR)) {
    			if(rightWhites.length() > 0) {
    				cv.append(rightWhites);
    				rightWhites = ""; //$NON-NLS-1$
    			}
				dv.append("\\"); //$NON-NLS-1$
    		} else {
				if(t.startsWith("#")) cv.append("\\"); //$NON-NLS-1$ //$NON-NLS-2$
				String app = t.trim();
				int off = t.indexOf(app);
				rightWhites = t.substring(off + app.length());
				cv.append(app);
				dv.append(t);
    		}
    	}
    	return value.equals(cv.toString()) ? dv.toString() : value;
    }
    
    public void edit(XModelObject object, String body) {
		XModelObject c = object.copy(0);
		XModelObjectLoaderUtil.setTempBody(c, body);
		load(c);
		object.fireObjectChanged(XModelTreeEvent.BEFORE_MERGE);
		try {
			merge(object, c);
		} finally {
			object.fireObjectChanged(XModelTreeEvent.AFTER_MERGE);
		}
    }
    
    private void merge(XModelObject o1, XModelObject o2) {
    	XModelObject[] c1 = o1.getChildren();
    	Map<String,XModelObject> m1 = new HashMap<String,XModelObject>();
    	for (int i = 0; i < c1.length; i++) m1.put(c1[i].getPathPart(), c1[i]);
		XModelObject[] c2 = o2.getChildren();
		RegularObjectImpl impl1 = (RegularObjectImpl)o1;
		boolean ch = c2.length != c1.length;
		boolean mod = false;
		for (int i = 0; i < c2.length; i++) {
			XModelObject c = (XModelObject)m1.remove(c2[i].getPathPart());
			if(c == null) {
				c = c2[i].copy();
				((XModelObjectImpl)c).setParent_0(impl1);
			} else if(!c.isEqual(c2[i])) { 
				mod = true;
				XAttribute[] as = c.getModelEntity().getAttributes();
				for (int j = 0; j < as.length; j++) {
					if(!as[j].isCopyable()) continue;				
					String n = as[j].getName();
					String v1 = c.getAttributeValue(n);
					String v2 = c2[i].getAttributeValue(n);
					if(v2 != null && !v2.equals(v1))
					  c.setAttributeValue(n, v2);				
				}
			}
			if(!ch && c1[i] != c) {
				ch = true;
			}
			c2[i] = c;
		}
		c1 = (XModelObject[])m1.values().toArray(new XModelObject[0]);
//		for (int i = 0; i < c1.length; i++) c1[i].removeFromParent();
		
		if(ch || c1.length > 0) {
			impl1.replaceChildren(c2);
			((XModelImpl)o1.getModel()).fireStructureChanged(o1);
		}

		String conclusion1 = o1.get("conclusion"); //$NON-NLS-1$
		if(conclusion1 == null) conclusion1 = ""; //$NON-NLS-1$
		String conclusion2 = o2.get("conclusion"); //$NON-NLS-1$
		if(conclusion2 == null) conclusion2 = ""; //$NON-NLS-1$
		if(!conclusion1.equals(conclusion2)) {
			o1.set("conclusion", conclusion2); //$NON-NLS-1$
			mod = true;
		}
		if(ch || mod) o1.setModified(true);
    }
    
	public static int getSeparatorIndex(String s) {
		String tr = s.trim();
		if(tr.length() == 0 || tr.charAt(0) == '#' || tr.charAt(0) == '!') return -1;
		boolean n = false;
		int firstWhiteSpace = -1;
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			if(Character.isWhitespace(c)) {
				if(n) {
					if(firstWhiteSpace < 0) firstWhiteSpace = i;
					//return i;
				}
				continue;
			} else if(c == '=' || c == ':') {
				return i;
			} else {
				if(n && firstWhiteSpace >= 0) return firstWhiteSpace;
				n = true;
			}
		}
		return s.length();
  	}

}
