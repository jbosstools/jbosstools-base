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
package org.jboss.tools.common.resref.core;

public class ResourceReference {
	public final static int FILE_SCOPE = 0;
	public final static int FOLDER_SCOPE = 1;
	public final static int PROJECT_SCOPE = 2;
	public final static int GLOBAL_SCOPE  = 3;
	
	public final  static String[] SCOPE_NAMES = new String[]{"Page", "Folder", "Project","Global"};

	protected String location;
	protected int scope;
	protected int depth = 0;
	protected String properties = "";
	
	protected boolean isGlobal = false;
	
	public boolean isGlobal() {
        return isGlobal;
    }

    public void setGlobal(boolean isGlobal) {
        this.isGlobal = isGlobal;
    }

    public ResourceReference(String location, int scope) {
		this.location = location;
		this.scope = scope;
		int q = location.indexOf('%');
		if(q >= 0) {
			properties = location.substring(q + 1);
			this.location = location.substring(0, q);
		}
	}
	
	public String getLocation() {
		return location;
	}
	
	public int getScope() {
		return scope;
	}
	
	public void setLocation(String location) {
		this.location = location;
	}
	
	public void setProperties(String properties) {
		this.properties = properties;
	}
	
	public String getProperties() {
		return properties;
	}
	
	public void setScope(int scope) {
		this.scope = scope;
	}
	
	public String getScopeName() {
		return SCOPE_NAMES[scope];
	}
	
	public void setDepth(int depth) {
		this.depth = depth;
	}
	
	public int getDepth() {
		return depth;
	}
	
	public String getLocationAndProperties() {
		String v = location;
		if(properties.length() > 0) {
			v += "%" + properties;
		}
		return v;
	}
	
	
//	public static ResourceReference createResourceReference(String location, int scope) {
//        ResourceReference rst = null;
//
//        switch (scope) {
//        case FILE_SCOPE:
//        case FOLDER_SCOPE:
//        case PROJECT_SCOPE:
//            rst = new ResourceReference(location, scope);
//            break;
//        case GLOBAL_SCOPE:
//            rst = new GlobalResourceReference(location, scope);
//            break;
//        default:
//            throw new IllegalArgumentException("Illegal scope=" + scope);
//        }
//        return rst;
//    }

}
