package org.jboss.tools.common.model.ui.navigator.decorator;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.StringTokenizer;

import org.eclipse.core.runtime.IConfigurationElement;
import org.jboss.tools.common.model.XModelObject;

public class XModelObjectDecorator {
	static String ATTR_NAME = "name";
	static String ATTR_PARTITION= "partition";
	static String ATTR_ENTITIES = "entities";
	static String ATTR_VARIABLES = "variables";
	static String ATTR_DEFAULT = "defaultValue";
	
	
	String name;
	String partition;
	Set<String> entities = new HashSet<String>();
	String[] variables = new String[0];
	String defaultValue;
	String value;
	
	List<DecoratorPart> parts = null;
	
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
	
	public String[] getVariables() {
		return variables;
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
			value = "{name}";
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
			String[] es = s.split(",");
			for (int i = 0; i < es.length; i++) entities.add(es[i]);
		}
		s = element.getAttribute(ATTR_VARIABLES);
		List<String> vs = new ArrayList<String>();
		vs.add("name");
		if(s != null) {
			String[] es = s.split(",");
			for (int i = 0; i < es.length; i++) vs.add(es[i]);
		}
		variables = vs.toArray(new String[0]);
	}
	
	List<DecoratorPart> compile() {
		if(this.parts != null) return this.parts;
		List<DecoratorPart> parts = new ArrayList<DecoratorPart>();
		String v = value;
		if(value == null || value.length() == 0) v = defaultValue;
		if(v == null) v = "";
		if(v.indexOf("{name}") < 0) {
			if(v.length() > 0) v += " ";
			v += "{name}";
		}
		StringTokenizer s = new StringTokenizer(v, "{}", true);
		boolean inVariable = false;
		while(s.hasMoreTokens()) {
			String t = s.nextToken();
			if(t.equals("{")) {
				inVariable = true;
			} else if(t.equals("}")) {
				inVariable = false;				
			} else if(inVariable) {
				if(t.equals("name")) {
					parts.add(new NameDecoratorPart());
				} else {
					parts.add(new AttributeDecoratorPart(t));
				}
			} else {
				parts.add(new DecoratorPart(t));
			}
		}
		return this.parts = parts;
	}
	
	public String getLabel(XModelObject object) {
		List<DecoratorPart> parts = compile();
		StringBuffer sb = new StringBuffer();
		for (DecoratorPart d: parts) {
			sb.append(d.getLabelPart(object));
		}
		return sb.toString();
	}

}
