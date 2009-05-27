/******************************************************************************* 
 * Copyright (c) 2007 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributors:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/

package org.jboss.tools.common.el.core;

import org.eclipse.core.runtime.QualifiedName;
import org.jboss.tools.common.resref.core.ResourceReferenceList;

/**
 * @author Evgenij Stherbin
 *
 */
public class GlobalELReferenceList extends ResourceReferenceList {
    /** The PROPERT y_ NAME. */
    private static QualifiedName PROPERTY_NAME = new QualifiedName("", "org.jboss.tools.vpe.editor.css.GlobalELReference"); //$NON-NLS-1$ //$NON-NLS-2$

    /** The instance. */
    private static GlobalELReferenceList instance = new GlobalELReferenceList();

    /**
     * Gets the instance.
     * 
     * @return the instance
     */
    public synchronized static GlobalELReferenceList getInstance() {
        return instance;
    }

    private GlobalELReferenceList() {
        super();
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
