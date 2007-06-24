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
import java.io.ByteArrayOutputStream;
import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.loaders.*;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.model.engines.impl.EnginesLoader;
import org.jboss.tools.common.model.impl.*;
import org.jboss.tools.common.meta.XAttribute;

public class PropertiesLoader implements XObjectLoader {
	static String INTERNAL_SEPARATOR = "@";
	String defaultLineSeparator = "\r\n";

    public PropertiesLoader() {}

    public void load(XModelObject object) {
        String body = XModelObjectLoaderUtil.getTempBody(object);
        Properties properties = new Properties();
        Properties mapping = new Properties();
        try {
        	ByteArrayInputStream s = new ByteArrayInputStream(body.getBytes());
			properties.load(s);
			Iterator it = properties.keySet().iterator();
			while(it.hasNext()) {
				String nm = it.next().toString();
				String sn = convertName(nm);
				mapping.put(sn, nm);
			}
        } catch (Exception e) {
        	//ignore
        }
        StringTokenizer st = new StringTokenizer(body, "\n\r", true);
        StringBuffer sb = new StringBuffer();
        StringBuffer lineEnd = new StringBuffer();
        int state = 0;
        XModelObject c = null;
        while(st.hasMoreTokens()) {
            String s = st.nextToken();
            if(s.equals("\r")) {
				if(state != 2) sb.append(s); else lineEnd.append(s);
            	continue;
            } 
            if(s.equals("\n")) {
                if(state < 2) sb.append(s); else lineEnd.append(s);
                if(state == 0) {
                	state = 1;
                } else if(state == 2) {
                	state = 0;
					c.setAttributeValue("dirtyvalue", sb.toString());
					c.setAttributeValue("line-end", lineEnd.toString());
					sb.setLength(0);
					lineEnd.setLength(0);
                } 
                continue;
            }
            lineEnd.setLength(0);
			if(state == 3) {
				if(!s.endsWith("\\")) {
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
            p.setProperty("name", visualName);
			p.setProperty("dirtyname", dirtyName);
            p.setProperty("value", value);
            
            p.setProperty("name-value-separator", i == s.length() ? " " : "" + s.charAt(i));
            p.setProperty("comments", comments);
            p.setProperty("separator", "#"); //obsolete
            p.setProperty("line-end", "");
            c = object.getModel().createModelObject("Property", p);
            object.addChild(c);
            String dirtyvalue = (i < s.length()) ? s.substring(i + 1) : "";
            if(s.endsWith("\\")) {
            	state = 3;
            	sb.append(dirtyvalue.substring(0, dirtyvalue.length() - 1)).append(INTERNAL_SEPARATOR);
            } else {
            	state = 2;
				sb.append(dirtyvalue);
            }
        }
        if(state == 1 && sb.length() > 0) {
        	object.set("conclusion", sb.toString()); 
        }
    }

    public boolean update(XModelObject object) {
        XModelObject c = object.copy(0);
		XModelObjectLoaderUtil.setTempBody(c, XModelObjectLoaderUtil.getTempBody(object));
        load(c);
        EnginesLoader.merge(object, c, false);
        object.setModified(false);
        return true;
    }

    public boolean save(XModelObject object) {
        if(!object.isModified()) return true;
        try {
			XModelObjectLoaderUtil.setTempBody(object, generateBody(object, defaultLineSeparator));
            object.setModified(true);
            return true;
        } catch (Exception exc) {
            return false;
        }
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
			String name_value_separator = cs[i].getAttributeValue("name-value-separator");
			if(name_value_separator == null || name_value_separator.length() != 1 || " \t=:".indexOf(name_value_separator) < 0) {
				name_value_separator = "=";
			}
			appendComments(sb, cs[i].get("COMMENTS"), cs[i].get("SEPARATOR"), lineSeparator);
			if("no".equals(cs[i].get("ENABLED"))) sb.append('#');
			String dirtyname = cs[i].getAttributeValue("dirtyname");
			String name = convertName(cs[i].get("NAME"));
			String value = cs[i].get("VALUE");
			String dirtyvalue = cs[i].getAttributeValue("dirtyvalue");
			if(value == null || dirtyvalue == null || !value.equals(dirtyvalue.trim())) {
				value = convertValue(value);
			}
			String resolved = resolveValue(value, dirtyvalue);
			if(dirtyname != null && name.equals(dirtyname.trim())) name = dirtyname; 
			sb.append(name);
			if(!" ".equals(name_value_separator) || resolved.length() > 0) {
				sb.append(name_value_separator);
			}
			sb.append(resolved);
			String ls = cs[i].get("line-end");
			if(ls.length() > 0) {
				if(ls.equals("\\r\\n")) ls = defaultLineSeparator;
				sb.append(ls);
			} else if(i < cs.length - 1) {
				ls = defaultLineSeparator;
				sb.append(ls);
			}
		}
		String conclusion = object.get("conclusion");
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
    	while(st.hasMoreTokens()) {
    		String t = st.nextToken();
    		if(t.equals(INTERNAL_SEPARATOR)) {
				dv.append("\\\n");
    		} else {
				if(t.startsWith("#")) cv.append("\\");
				cv.append(t.trim());
				dv.append(t);
    		}
    	}
    	return value.equals(cv.toString()) ? dv.toString() : value;
    }
    
    public void edit(XModelObject object, String body) {
		XModelObject c = object.copy(0);
		XModelObjectLoaderUtil.setTempBody(c, body);
		load(c);
		merge(object, c);    	
    }
    
    private void merge(XModelObject o1, XModelObject o2) {
    	XModelObject[] c1 = o1.getChildren();
    	Map<String,XModelObject> m1 = new HashMap<String,XModelObject>();
    	for (int i = 0; i < c1.length; i++) m1.put(c1[i].getPathPart(), c1[i]);
		XModelObject[] c2 = o2.getChildren();
		RegularObjectImpl impl1 = (RegularObjectImpl)o1;
		boolean ch = false;
		for (int i = 0; i < c2.length; i++) {
			XModelObject c = (XModelObject)m1.remove(c2[i].getPathPart());
			if(c == null) {
				o1.addChild(c = c2[i].copy());
				ch = true;				
			} else if(!c.isEqual(c2[i])) { 
				ch = true;
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
			int ic = impl1.getIndexOfChild(c);
			if(ic != i) {
				impl1.move(ic, i, true);				
			}			
		}
		c1 = (XModelObject[])m1.values().toArray(new XModelObject[0]);
		for (int i = 0; i < c1.length; i++) c1[i].removeFromParent();
		String conclusion1 = o1.get("conclusion");
		if(conclusion1 == null) conclusion1 = "";
		String conclusion2 = o2.get("conclusion");
		if(conclusion2 == null) conclusion2 = "";
		if(!conclusion1.equals(conclusion2)) {
			o1.set("conclusion", conclusion2);
			ch = true;
		}
		if(ch) o1.setModified(true);
    }
    
    private String convertName(String n) {
    	Properties p = new Properties();
    	p.setProperty(n, "value");
    	ByteArrayOutputStream os = new ByteArrayOutputStream();
    	try {
    		p.store(os, null);
    	} catch (Exception e) {
    		ModelPlugin.log(e);
    	}
    	String q = os.toString();
    	int i = q.indexOf("=value");
    	q = q.substring(0, i);
    	i = q.lastIndexOf('\n');
    	if(i >= 0) q = q.substring(i + 1);
    	return q.trim();
    }
    
    private String convertValue(String v) {
    	Properties p = new Properties();
    	p.setProperty("name", v);
    	ByteArrayOutputStream os = new ByteArrayOutputStream();
    	try {
    		p.store(os, null);
    	} catch (Exception e) {
    		ModelPlugin.log(e);
    	}
    	String q = os.toString();
    	int i = q.indexOf("name=");
    	q = q.substring(i + 5);
    	while(q.endsWith("\n")) q = q.substring(0, q.length() - 1);
    	while(q.endsWith("\r")) q = q.substring(0, q.length() - 1);
    	return q;
    }

	private static int getSeparatorIndex(String s) {
		String tr = s.trim();
		if(tr.length() == 0 || tr.charAt(0) == '#' || tr.charAt(0) == '!') return -1;
		boolean n = false;
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			if(Character.isWhitespace(c)) {
				if(n) return i;
				continue;
			} else if(c == '=' || c == ':') {
				return i;
			} else {
				n = true;
			}
		}
		return s.length();
  	}

}
