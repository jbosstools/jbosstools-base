/******************************************************************************* 
 * Copyright (c) 2008 Red Hat, Inc. 
 * Distributed under license by Red Hat, Inc. All rights reserved. 
 * This program is made available under the terms of the 
 * Eclipse Public License v1.0 which accompanies this distribution, 
 * and is available at http://www.eclipse.org/legal/epl-v10.html 
 * 
 * Contributors: 
 * Red Hat, Inc. - initial API and implementation 
 ******************************************************************************/ 
package org.jboss.tools.common.el.core;

import org.eclipse.core.runtime.QualifiedName;
import org.jboss.tools.common.resref.core.ResourceReferenceList;

/**
 * The Class ELReferenceList.
 */
public class ELReferenceList extends ResourceReferenceList {
    
    /** The PROPERT y_ NAME. */
    private static QualifiedName PROPERTY_NAME = new QualifiedName("", "org.jboss.tools.vpe.editor.css.ELReference");

    /** The instance. */
    static ELReferenceList instance = new ELReferenceList();

    /**
     * Gets the instance.
     * 
     * @return the instance
     */
    public static ELReferenceList getInstance() {
        return instance;
    }

    /**
     * Gets the property name.
     * 
     * @return the property name
     */
    protected QualifiedName getPropertyName() {
        return PROPERTY_NAME;
    }
}
