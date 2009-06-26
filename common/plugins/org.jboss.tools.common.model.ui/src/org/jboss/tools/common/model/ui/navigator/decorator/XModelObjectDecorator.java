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
package org.jboss.tools.common.model.ui.navigator.decorator;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.StringTokenizer;

import org.eclipse.core.runtime.IConfigurationElement;
import org.jboss.tools.common.model.XModelObject;

/**
 * @author Viacheslav Kabanovich
 */
public class XModelObjectDecorator implements DecoratorConstants {
	String name;
	String partition;
	Set<String> entities = new HashSet<String>();
	Variable[] variables = new Variable[0];
	Map<String, Variable> variableByName = new HashMap<String, Variable>();
	String defaultValue;
	String value;
	
	List<IDecoratorPart> parts = null;
	
	XModelObject[] examples = new XModelObject[0];
	
	public XModelObjectDecorator() {}
	
	public String getName() {
		return name;
	}
	
	public String getPartition() {
		return partition;
	}
	
	public Set<String> getEntities() {
		return entities;
	}
	
	public Variable[] getVariables() {
		return variables;
	}
	
	public Variable getVariableByName(String name) {
		return variableByName.get(name);
	}
	
	public XModelObject[] getExamples() {
		return examples;
	}
	
	public String getDefaultValue() {
		return defaultValue;
	}
	
	public String getValue() {
		return value;
	}
	
	public void setValue(String value) {
		if(value != null && value.equals(this.value)) {
			return;
		}
		this.value = value;
		if(value == null || value.length() == 0) {
			value = Variable.NAME.getRuleText();
		}
		parts = null;
	}
	
	void load(IConfigurationElement element) {
		name = element.getAttribute(ATTR_NAME);
		partition = element.getAttribute(ATTR_PARTITION);
		defaultValue = element.getAttribute(ATTR_DEFAULT);
		setValue(defaultValue);
		value = defaultValue;
		String s = element.getAttribute(ATTR_ENTITIES);
		if(s != null) {
			String[] es = s.split(","); //$NON-NLS-1$
			for (int i = 0; i < es.length; i++) entities.add(es[i]);
		}
		IConfigurationElement[] cs = element.getChildren(NODE_VARIABLE);
		List<Variable> vs = new ArrayList<Variable>();
		vs.add(Variable.NAME);
		variableByName.put(Variable.NAME.getName(), Variable.NAME);
		for (int i = 0; i < cs.length; i++) {
			Variable v = new Variable();
			v.load(cs[i]);
			vs.add(v);
			variableByName.put(v.getName(), v);
		}
		variables = vs.toArray(new Variable[0]);
		
		List<XModelObject> es = new ArrayList<XModelObject>();
		cs = element.getChildren(NODE_EXAMPLE);
		for (int i = 0; i < cs.length; i++) {
			XModelObject o = Example.load(cs[i]);
			if(o != null) es.add(o);
		}
		examples = es.toArray(new XModelObject[0]);
	}
	
	List<IDecoratorPart> compile() {
		if(this.parts != null) return this.parts;
		List<IDecoratorPart> parts = new ArrayList<IDecoratorPart>();
		String v = value;
		if(value == null || value.length() == 0) v = defaultValue;
		if(v == null) v = ""; //$NON-NLS-1$
		if(v.indexOf(RULE_OPENING) < 0) {
			if(v.length() > 0) v = " " + v; //$NON-NLS-1$
			v = Variable.NAME.getRuleText() + v;
		}
		StringTokenizer s = new StringTokenizer(v, RULE_OPENING + RULE_CLOSING, true);
		boolean inVariable = false;
		while(s.hasMoreTokens()) {
			String t = s.nextToken();
			if(t.equals(RULE_OPENING)) {
				inVariable = true;
			} else if(t.equals(RULE_CLOSING)) {
				inVariable = false;				
			} else if(inVariable) {
				String n = t;
				String p = ""; //$NON-NLS-1$
				int i = t.indexOf('(');
				int j = t.indexOf(')');
				if(i >= 0 && j > i) {
					n = t.substring(0, i);
					p = t.substring(i + 1, j);
				}
				Variable variable = getVariableByName(n);
				if(variable == Variable.NAME) {
					parts.add(NameDecoratorPart.INSTANCE);
				} else if(variable != null){
					AttributeDecoratorPart part = new AttributeDecoratorPart(variable);
					part.setParameters(p);
					parts.add(part);
				} else {
					parts.add(new DecoratorPart(RULE_OPENING + t + RULE_CLOSING));
				}
			} else {
				parts.add(new DecoratorPart(t));
			}
		}
		boolean hasSignificantPart = false;
		for (IDecoratorPart p: parts) {
			if(p != NameDecoratorPart.INSTANCE && !(p instanceof AttributeDecoratorPart)) continue;
			hasSignificantPart = true;
			break;
		}
		if(!hasSignificantPart) {
			parts.add(NameDecoratorPart.INSTANCE);
		}
		return this.parts = parts;
	}
	
	public String getLabel(XModelObject object) {
		List<IDecoratorPart> parts = compile();
		StringBuffer sb = new StringBuffer();
		for (IDecoratorPart d: parts) {
			sb.append(d.getLabelPart(object));
		}
		String s = sb.toString().trim();
		if(s.length() == 0) s = object.getPresentationString();
		return s;
	}
	
	/**
	 * Needed only to work with value
	 * @return
	 */
	public XModelObjectDecorator getWorkingCopy() {
		XModelObjectDecorator copy = new XModelObjectDecorator();
		copy.name = name;
		copy.partition = partition;
		copy.defaultValue = defaultValue;
		copy.value = value;
		copy.entities = entities;
		copy.variables = variables;
		copy.variableByName = variableByName;
		
		return copy;
	}

}
