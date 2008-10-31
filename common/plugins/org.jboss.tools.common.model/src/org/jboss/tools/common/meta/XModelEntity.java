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
package org.jboss.tools.common.meta;

import org.jboss.tools.common.meta.action.XActionList;
import org.jboss.tools.common.model.XModelObject;
import org.jboss.tools.common.model.loaders.XObjectLoader;

public interface XModelEntity extends XMetaElement, XDependencies {

	public String getModule();

    public XAttribute[] getAttributes();
    public XAttribute getAttribute(String name);
    
    /**
     * 
     * @deprecated
     * @return
     */
    public Class getImplementingClass();

    public boolean hasObjectImplementation();
    public XModelObject getObjectImplementation();
    public boolean hasObjectLoader();
    public XObjectLoader getObjectLoader();
    public String getGeneratorClassName();
    public String getEditorClassName();
    public XActionList getActionList();
    public XChild[] getChildren();
    public XChild getChild(String entityName);
    public XEntityRenderer getRenderer();
    public String getXMLSubPath();
    public XAdoptManager getAdoptManager();
	public String getProperty(String name);

    //optimization
    public int getPropertyIndex(String name, boolean register);
    public int getPropertyCount();
    public String getChildByXML(String xmlname);
    public java.util.HashSet<String> getRequiredChildren();
    
    public String testImplementation();
    public String testLoader();
}

