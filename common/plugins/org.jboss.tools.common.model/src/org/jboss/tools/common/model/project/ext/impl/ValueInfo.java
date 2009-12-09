/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 

package org.jboss.tools.common.model.project.ext.impl;

import java.util.List;
import java.util.Properties;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.jdt.core.IAnnotation;
import org.eclipse.jdt.core.IMemberValuePair;
import org.eclipse.jdt.core.ISourceRange;
import org.eclipse.jdt.core.dom.Annotation;
import org.eclipse.jdt.core.dom.Expression;
import org.eclipse.jdt.core.dom.MemberValuePair;
import org.eclipse.jdt.core.dom.NormalAnnotation;
import org.eclipse.jdt.core.dom.QualifiedName;
import org.eclipse.jdt.core.dom.SingleMemberAnnotation;
import org.eclipse.jdt.core.dom.StringLiteral;
import org.jboss.tools.common.model.plugin.ModelPlugin;
import org.jboss.tools.common.model.project.ext.IValueInfo;
import org.jboss.tools.common.model.project.ext.store.XMLStoreConstants;
import org.jboss.tools.common.xml.XMLUtilities;
import org.w3c.dom.Element;

public class ValueInfo implements IValueInfo {
	String value;
	public int valueStartPosition;
	public int valueLength;
	
	/**
	 * Factory method.
	 * @param node
	 * @param name
	 * @return
	 */
	public static ValueInfo getValueInfo(Annotation node, String name) {
		if(name == null) name = "value"; //$NON-NLS-1$
		if(node instanceof SingleMemberAnnotation) {
			if(name == null || "value".equals(name)) { //$NON-NLS-1$
				SingleMemberAnnotation m = (SingleMemberAnnotation)node;
				ValueInfo result = new ValueInfo();
				Expression exp = m.getValue();
				result.valueLength = exp.getLength();
				result.valueStartPosition = exp.getStartPosition();
				result.value = checkExpression(exp);				
				return result;
			}
			return null;
		} else if(node instanceof NormalAnnotation) {
			NormalAnnotation n = (NormalAnnotation)node;
			List<?> vs = n.values();
			if(vs != null) for (int i = 0; i < vs.size(); i++) {
				MemberValuePair p = (MemberValuePair)vs.get(i);
				String pname = p.getName().getIdentifier();
				if(!name.equals(pname)) continue;
				ValueInfo result = new ValueInfo();
				Expression exp = p.getValue();
				result.valueLength = exp.getLength();
				result.valueStartPosition = exp.getStartPosition();
				result.value = checkExpression(exp);				
				return result;
			}
			return null;			
		}
		return null;		
	}

	public static ValueInfo getValueInfo(IAnnotation annotation, String name) {
		if(name == null) name = "value"; //$NON-NLS-1$
		ValueInfo result = new ValueInfo();
		ISourceRange r = null;
		IMemberValuePair[] ps = null;
		try {
			r = annotation.getSourceRange();
			ps = annotation.getMemberValuePairs();
		} catch (CoreException e) {
			ModelPlugin.getDefault().logError(e);
		}
		if(r != null) {
			result.valueStartPosition = r.getOffset();
			result.valueLength = r.getLength();
		}
		if(ps != null) for (IMemberValuePair p: ps) {
			if(name.equals(p.getMemberName())) {
				Object v = p.getValue();
				if(v != null) result.value = v.toString();
				break;
			}
		}
		return result;
	}
	
	public ValueInfo() {
	}
	
	public String getValue() {
		return value;
	}
	
	public int getStartPosition() {
		return valueStartPosition;
	}
	
	public int getLength() {
		return valueLength;
	}

	static String checkExpression(Expression exp) {
		if(exp == null) return null;
		if(exp instanceof StringLiteral) {
			return ((StringLiteral)exp).getLiteralValue();
		} else if(exp instanceof QualifiedName) {
			Object o = exp.resolveConstantExpressionValue();
			if(o != null) return o.toString();
			return exp.toString();
		}
		Object o = exp.resolveConstantExpressionValue();
		if(o != null) return o.toString();
		return exp.toString();
	}
	
	public void setValue(String value) {
		this.value = value;
	}

	public Element toXML(Element parent, Properties context) {
		Element element = XMLUtilities.createElement(parent, XMLStoreConstants.TAG_VALUE_INFO);
		if(value != null) element.setAttribute(XMLStoreConstants.ATTR_VALUE, value);
		if(valueStartPosition != 0) element.setAttribute(ATTR_START, "" + valueStartPosition); //$NON-NLS-1$
		if(valueLength != 0) element.setAttribute(ATTR_LENGTH, "" + valueLength); //$NON-NLS-1$
		return element;
	}
	
	static String ATTR_START = "start"; //$NON-NLS-1$
	static String ATTR_LENGTH = "length"; //$NON-NLS-1$

	public void loadXML(Element element, Properties context) {
		value = element.getAttribute(XMLStoreConstants.ATTR_VALUE);
		String start = element.getAttribute(ATTR_START);
		if(start != null && start.length() > 0) {
			try {
				valueStartPosition = Integer.parseInt(start);
			} catch (NumberFormatException e) {
				//ignore
			}
		}
		String length = element.getAttribute(ATTR_LENGTH);
		if(length != null && length.length() > 0) {
			try {
				valueLength = Integer.parseInt(length);
			} catch (NumberFormatException e) {
				//ignore
			}
		}
	}
}
