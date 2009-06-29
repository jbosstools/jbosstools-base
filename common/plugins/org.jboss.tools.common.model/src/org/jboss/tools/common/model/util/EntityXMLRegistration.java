package org.jboss.tools.common.model.util;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import org.eclipse.wst.common.uriresolver.internal.ExtensibleURIResolver;
import org.eclipse.wst.xml.core.internal.XMLCorePlugin;
import org.jboss.tools.common.meta.XAttribute;
import org.jboss.tools.common.meta.XModelEntity;
import org.jboss.tools.common.xml.XMLEntityResolver;

public class EntityXMLRegistration {
	private static EntityXMLRegistration instance = new EntityXMLRegistration();
	
	public static int UNRESOLVED = -1;
	public static int DTD = 0;
	public static int SCHEMA = 1;
	public static int MISSING = 2;
	
	static boolean isResolvingSchema = false;
	
	private EntityXMLRegistration() {}
	
	public static EntityXMLRegistration getInstance() {
		return instance;
	}
	
	private Map<XModelEntity, Integer> resolved = new HashMap<XModelEntity, Integer>();
	
	public int resolve(XModelEntity entity) {
		Integer i = resolved.get(entity);
		if(i != null) return i.intValue();
		
		XAttribute a = entity.getAttribute("publicId"); //$NON-NLS-1$
		if(a != null) {
			return resolveDTD(entity, a);
		}
		a = entity.getAttribute("xsi:schemaLocation"); //$NON-NLS-1$
		if(a != null && isResolvingSchema) {
			return resolveSchema(entity, a);
		}
		resolved.put(entity, Integer.valueOf(UNRESOLVED));
		return UNRESOLVED;
	}
	
	private int resolveDTD(XModelEntity entity, XAttribute a) {
		String v = a.getDefaultValue();
		if(XMLEntityResolver.getInstance().isResolved(v, null)) {
			resolved.put(entity, Integer.valueOf(DTD));
			return DTD;
		}
		ExtensibleURIResolver r = new ExtensibleURIResolver();
		String s = r.resolve(null, v, null);
		if(s != null && s.length() > 0) {
			resolved.put(entity, Integer.valueOf(DTD));
			XMLEntityResolver.registerPublicEntity(v, s);
			return DTD;
		}
		resolved.put(entity, Integer.valueOf(MISSING));
		return MISSING;
	}

	private int resolveSchema(XModelEntity entity, XAttribute a) {
		String v = a.getDefaultValue();
		String[] vs = v.split(" "); //$NON-NLS-1$
		if(vs == null || vs.length < 2) {
			resolved.put(entity, Integer.valueOf(MISSING));
			return MISSING;
		}
		String location = null;
		try {
			location = XMLCorePlugin.getDefault().getDefaultXMLCatalog().resolvePublic(vs[0], vs[1]);
			if(location == null) location = XMLCorePlugin.getDefault().getDefaultXMLCatalog().resolveSystem(vs[1]);
			if(location == null) location = XMLCorePlugin.getDefault().getDefaultXMLCatalog().resolveURI(vs[1]);
		} catch (IOException e) {
			//ignore
		}
		if(location == null) {
			ExtensibleURIResolver r = new ExtensibleURIResolver();
			location = r.resolve(null, vs[0], vs[1]);
		}
		if(location != null && location.length() > 0) {
			resolved.put(entity, Integer.valueOf(SCHEMA));
			return SCHEMA;
		}
		return SCHEMA;
	}

    public static boolean isSystemId(String body) {
    	if(body == null) return false;
        int i = body.indexOf("<!DOCTYPE"); //$NON-NLS-1$
        if(i < 0) return false;
        int j = body.indexOf(">", i); //$NON-NLS-1$
        if(j < 0) return false;
        String dt = body.substring(i, j);
        return (dt.indexOf("SYSTEM") > 0);         //$NON-NLS-1$
    }
    
}
