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
//	static String INTERNAL_SEPARATOR = "\\"; //$NON-NLS-1$
	static String defaultLineSeparator;
	
	static {
		defaultLineSeparator = System.getProperty("line.separator"); //$NON-NLS-1$
		if(defaultLineSeparator == null) {
			defaultLineSeparator = "\r\n"; //$NON-NLS-1$
		}
	}

    public PropertiesLoader() {}

    public static String getEncoding(XModelObject object) {
    	if(!object.isActive()) {
    		String encoding = object.get("_encoding_"); //$NON-NLS-1$
    		return encoding != null && encoding.length() > 0 ? encoding : "8859_1"; //$NON-NLS-1$
    	}
    	IFile f = getFile(object);
    	return f != null ? FileUtil.getEncoding(f) : null;
    }
   
    private static IFile getFile(XModelObject object) {
    	Object resource = object.getAdapter(IResource.class);
    	return (resource instanceof IFile) ? (IFile)resource : null;    	
    }

    /**
	  * We scan body of file as set of tokens
	  * 	- line without symbols breaking line;
	  * 	- \r
	  * 	- \n
	  * While scanning tokens, depending on what is already processed we put 
	  * our engine into one of possible states:
	  * 	state = 0 - we are about to read a new property. That is the case 
	  * 				in the beginning and after reading of some property is 
	  * 				completed.
	  * 	state = 1 - we have read something while being in state 0, that 
	  * 				does not contain property. We do not have a property 
	  * 				object yet to bind that symbol. We have to keep that 
	  * 				state until we get a token that contains a property. 
	  * 				If stream ends before that, all the accumulated text 
	  * 				will be stored as a special property of file object.
	  * 	state = 2 - we have read a line that contains a property and the 
	  * 				line is not concluded with symbol of property 
	  * 				continuation '\'. Now we have to wait for line breaking 
	  * 				symbols to complete processing the current property. 
	  * 				After that, state will be set to 0.
	  * 	state = 3 - we have read a line that contains a property and the 
	  * 				line is concluded with symbol of property 
	  * 				continuation '\'. We have to continue to read the 
	  * 				current property.
	  * 	state = 4 - while in state 3, we have read line breaking symbols. 
	  * 				We have to note that, because, if next token will be 
	  * 				again line breaking symbols, that will mean that the 
	  * 				current property is completed.
	  * These 4 states are the complete set to describe the process of 
	  * splitting properties file into property objects.
     */
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
            	if(lineEnd.toString().equals("\r")) { //$NON-NLS-1$
                    if(state == 0) {
                    	state = 1;
                    } else if(state == 2) {
                    	state = 0;
    					c.setAttributeValue("dirtyvalue", sb.toString()); //$NON-NLS-1$
    					c.setAttributeValue("line-end", lineEnd.toString()); //$NON-NLS-1$
    					sb.setLength(0);
    					lineEnd.setLength(0);
                    } 
            	}
				if(state != 2 && state != 4) sb.append(s); else lineEnd.append(s);
            	continue;
            } 
            if(s.equals("\n")) { //$NON-NLS-1$
                if(state != 2) {
                	if(state != 4) {
                		sb.append(s);
                	} else {
                		lineEnd.append(s);
                		state = 2;
                	}
                	if(state == 3) {
                		state = 4;
                	}
                } else lineEnd.append(s);
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
			if(state == 3 || state == 4) {
				if(!endsWithBackslash(s)) {
					sb.append(s);
					state = 2;
				} else {
					sb.append(s);
					state = 3;
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
            	XModelObject q = object.getChildByPath(c.getPathPart());
            	if(q != null) {
            		int k = 1;
            		while(true) {
            			c.set(XModelObjectImpl.DUPLICATE, "" + k);
            			if(object.getChildByPath(c.getPathPart()) == null) break;
            			k++;
            		}
            		q.set(XModelObjectImpl.DUPLICATE, "" + k);
            		q.setAttributeValue("value", convertDirtyValue(q.getAttributeValue("dirtyvalue"))); 
            		c.set(XModelObjectImpl.DUPLICATE, "");

            	}
            object.addChild(c);

            String dirtyvalue = (i < s.length()) ? s.substring(i + 1) : ""; //$NON-NLS-1$
            if(endsWithBackslash(s)) {
            	state = 3;
            	sb.append(dirtyvalue);
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

    boolean endsWithBackslash(String s) {
    	boolean result = false;
    	for (int i = s.length() - 1; i >= 0; i--) {
    		if(s.charAt(i) != '\\') return result;
    		result = !result;
    	}
    	return result;
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
		XModelObjectLoaderUtil.setTempBody(object, generateBody(object));
        object.setModified(true);
        return true;
    }

    private void appendComments(StringBuffer sb, String comments, String commentSeparator, String lineSeparator) {
        if(comments.length() == 0) return;
        sb.append(comments);
    }

	public String getBody(XModelObject object) {
		return generateBody(object);	
	}

    private String generateBody(XModelObject object) {
    	String lineSeparator = defaultLineSeparator;
		StringBuffer sb = new StringBuffer();
		XModelObject[] cs = object.getChildren();
		for (int i = 0; i < cs.length; i++) {
			String ls = cs[i].get("line-end"); //$NON-NLS-1$
			if(ls.length() > 0 && !ls.equals("\\r\\n")) { //$NON-NLS-1$
				lineSeparator = ls;
				break;
			}
		}
		for (int i = 0; i < cs.length; i++) {
			String name_value_separator = cs[i].getAttributeValue("name-value-separator"); //$NON-NLS-1$
			if(name_value_separator == null || name_value_separator.length() != 1 || " \t=:".indexOf(name_value_separator) < 0) { //$NON-NLS-1$
				name_value_separator = "="; //$NON-NLS-1$
			}
			appendComments(sb, cs[i].get("COMMENTS"), cs[i].get("SEPARATOR"), lineSeparator); //$NON-NLS-1$ //$NON-NLS-2$
			if(XModelObjectConstants.NO.equals(cs[i].get("ENABLED"))) sb.append('#'); //$NON-NLS-1$
			String dirtyname = cs[i].getAttributeValue("dirtyname"); //$NON-NLS-1$
			String name = EncodedProperties.saveConvert(cs[i].get(XModelObjectConstants.XML_ATTR_NAME), true);
			String value = cs[i].get("VALUE"); //$NON-NLS-1$
			String dirtyvalue = cs[i].getAttributeValue("dirtyvalue"); //$NON-NLS-1$
			if(value == null || dirtyvalue == null || !value.equals(trimLeft(dirtyvalue))) {
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
				if(ls.equals("\\r\\n")) ls = lineSeparator; //$NON-NLS-1$
				sb.append(ls);
			} else if(i < cs.length - 1) {
				ls = lineSeparator;
				sb.append(ls);
			}
		}
		String conclusion = object.get("conclusion"); //$NON-NLS-1$
		if(conclusion != null) sb.append(conclusion);
		return sb.toString();    	
    }
   
    private String trimLeft(String s) {
		int qq = 0;
		while(qq < s.length() && s.charAt(qq) <= ' ') qq++;
		return (qq > 0) ? s.substring(qq) : s;
    }
    
    String resolveValue(String value, String dirtyvalue) {
    	if(dirtyvalue == null) return value;
    	if(trimLeft(dirtyvalue).equals(value)) return dirtyvalue;
    	String cv = convertDirtyValue(value);
    	String cdv = convertDirtyValue(dirtyvalue);
    	
    	if(cv != null && cv.equals(cdv)) {
    		return dirtyvalue;
    	}

    	List<PropertyValueToken> t1 = new PropertyValueParser(dirtyvalue).tokens;
    	List<PropertyValueToken> t2 = new PropertyValueParser(value).tokens;
    	int i1 = 0;
    	int i2 = 0;
    	while(true) {
    		while(i1 < t1.size() && t1.get(i1).value.length() == 0) i1++;
    		while(i2 < t2.size() && t2.get(i2).value.length() == 0) i2++;
    		if(i1 < t1.size() && i2 < t2.size() && t1.get(i1).value.equals(t2.get(i2).value)) {
    			i1++;
    			i2++;
    		} else break;
    	}
    	int i3 = t1.size() - 1, i4 = t2.size() - 1;
    	while(true) {
    		while(i3 >= 0 && t1.get(i3).value.length() == 0) i3--;
    		while(i4 >= 0 && t2.get(i4).value.length() == 0) i4--;
    		if(i3 >= 0 && i4 >= 0 && t1.get(i3).value.equals(t2.get(i4).value)) {
    			i3--;
    			i4--;
    		} else break;
    	}
    	List<PropertyValueToken> t = new ArrayList<PropertyValueToken>();
    	for (int i = 0; i < i1; i++) t.add(t1.get(i));
    	for (int i = i2; i <= i4; i++) t.add(t2.get(i));
    	if(i3 < i1) i3 = i1 - 1;
    	for (int i = i3 + 1; i < t1.size(); i++) t.add(t1.get(i));
    	StringBuilder result = new StringBuilder();
    	for (PropertyValueToken q: t) result.append(q.source);
    	
    	return result.toString();// value;
    }

    String convertDirtyValue(String dirtyvalue) {
    	ByteArrayInputStream sr = new ByteArrayInputStream(("a=" + dirtyvalue + "\nb=v").getBytes());
    	Properties p = new Properties();
    	try {
    		p.load(sr);
    	} catch (IOException e) {
    		//ignore
    	} catch (IllegalArgumentException e1) {
    		return null;
    	}
    	return p.getProperty("a");
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
		boolean backslash = false;
		int firstWhiteSpace = -1;
		for (int i = 0; i < s.length(); i++) {
			char c = s.charAt(i);
			if(backslash && (c == ' ' || c == 't' || c == 'n')) {
				// do nothing
			} else if(Character.isWhitespace(c)) {
				if(n) {
					if(firstWhiteSpace < 0) firstWhiteSpace = i;
					//return i;
				}
				continue;
			} else if(c == '=' || c == ':') {
				if(!backslash) return i;
			} else {
				if(n && firstWhiteSpace >= 0) return firstWhiteSpace;
				n = true;
			}
			if(c == '\\') {
				backslash = !backslash;
			} else {
				backslash = false;
			}
		}
		return s.length();
  	}

}

class PropertyValueParser {
	static int SKIP_SPACES = 0;
	
	String value;
	int offset;
	List<PropertyValueToken> tokens;
	
	public PropertyValueParser(String value) {
		this.value = value;
		parse();
	}

	public String getValue() {
		StringBuilder sb = new StringBuilder();
		for (PropertyValueToken t: tokens) {
			sb.append(t.value);
		}
		return sb.toString();
	}

	public String getSource() {
		StringBuilder sb = new StringBuilder();
		for (PropertyValueToken t: tokens) {
			sb.append(t.source);
		}
		return sb.toString();
	}

	void parse() {
		tokens = new ArrayList<PropertyValueToken>();
		skipSpaces();
		boolean backSlash = false;
		while(offset < value.length()) {
			char ch = value.charAt(offset);
			if(ch == '\\') {
				offset++;
				backSlash = !backSlash;
				if(!backSlash) {
					tokens.add(new PropertyValueToken(value.substring(offset - 2, offset), "\\"));
				}				
			} else {
				if(backSlash) {
					backSlash = false;
					if(ch == '\r' || ch == '\n') {
						tokens.add(new PropertyValueToken(value.substring(offset - 1, offset), ""));
						skipSpaces();
						continue;
					} else if(ch == 't') {
						tokens.add(new PropertyValueToken(value.substring(offset - 1, offset + 1), "\t"));
						offset++;
					} else if(ch == 'f') {
						tokens.add(new PropertyValueToken(value.substring(offset - 1, offset + 1), "\f"));
						offset++;
					} else if(ch == 'r') {
						tokens.add(new PropertyValueToken(value.substring(offset - 1, offset + 1), "\r"));
						offset++;
					} else if(ch == 'n') {
						tokens.add(new PropertyValueToken(value.substring(offset - 1, offset + 1), "\n"));
						offset++;
					} else if(ch == 'u') {
						offset++;
						int code = 0;
						for (int i = offset; i < offset + 4; i++) {
							char ch1 = value.charAt(i);
							int q = 0;
							if(ch1 >= '0' && ch1 <= '9') {
								q = (int)(ch1 - '0');
							} else if(ch1 >= 'a' && ch1 <= 'f') {
								q = 10 + (int)(ch1 - 'a');
							} else if(ch1 >= 'A' && ch1 <= 'F') {
								q = 10 + (int)(ch1 - 'A');
							}
							code = code * 16 + q;
						}
						tokens.add(new PropertyValueToken(value.substring(offset - 2, offset + 4), "" + (char)code));
						offset += 4;
					} else {
						tokens.add(new PropertyValueToken(value.substring(offset - 1, offset + 1), "" + ch));
						offset++;
					}
				} else {
					tokens.add(new PropertyValueToken(value.substring(offset, offset + 1), "" + ch));
					offset++;
				}
			}
		}
		if(backSlash) {
			tokens.add(new PropertyValueToken(value.substring(offset - 1, offset), ""));
		}				
	}

	void skipSpaces() {
		int off = offset;
		while(off < value.length() && Character.isWhitespace(value.charAt(off))) {
			off++;
		}
		if(off > offset) {
			tokens.add(new PropertyValueToken(value.substring(offset, off), ""));
			offset = off;
		}
	}
	
}

class PropertyValueToken {
	String source;
	String value;
	PropertyValueToken(String source, String value) {
		this.source = source;
		this.value = value;
	}
	
}