package org.jboss.tools.common.model.util;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.wst.common.uriresolver.internal.ExtensibleURIResolver;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.XModelEntity;
import org.jboss.tools.common.xml.XMLEntityResolver;

public class EntityXMLRegistration {
	private static EntityXMLRegistration instance = new EntityXMLRegistration();
	
	public static int UNRESOLVED = -1;
	public static int DTD = 0;
	public static int SCHEMA = 1;
	public static int MISSING = 2;
	
	private EntityXMLRegistration() {}
	
	public static EntityXMLRegistration getInstance() {
		return instance;
	}
	
	private Map<XModelEntity, Integer> resolved = new HashMap<XModelEntity, Integer>();
	
	public int resolve(XModelEntity entity) {
		Integer i = resolved.get(entity);
		if(i != null) return i.intValue();
		
		XAttribute a = entity.getAttribute("publicId");
		if(a != null) {
			return resolveDTD(entity, a);
		}
		a = entity.getAttribute("xsi:schemaLocation");
		if(a != null) {
//			return resolveSchema(entity, a);
		}
		resolved.put(entity, new Integer(UNRESOLVED));
		return UNRESOLVED;
	}
	
	private int resolveDTD(XModelEntity entity, XAttribute a) {
		String v = a.getDefaultValue();
		if(XMLEntityResolver.getInstance().isResolved(v, null)) {
			resolved.put(entity, new Integer(DTD));
			return DTD;
		}
		ExtensibleURIResolver r = new ExtensibleURIResolver();
		String s = r.resolve(null, v, null);
		if(s != null && s.length() > 0) {
			resolved.put(entity, new Integer(DTD));
			XMLEntityResolver.registerPublicEntity(v, s);
			return DTD;
		}
		resolved.put(entity, new Integer(MISSING));
		return MISSING;
	}

	private int resolveSchema(XModelEntity entity, XAttribute a) {
		String v = a.getDefaultValue();
		String[] vs = v.split(" ");
		if(vs == null || vs.length < 2) {
			resolved.put(entity, new Integer(MISSING));
			return MISSING;
		}
		ExtensibleURIResolver r = new ExtensibleURIResolver();
		String s = r.resolve(null, vs[0], vs[1]);
		if(s != null && s.length() > 0) {
			resolved.put(entity, new Integer(SCHEMA));
			return SCHEMA;
		}
		return SCHEMA;
	}

}
