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
package org.jboss.tools.common.meta.action;

public interface XActionList extends XActionItem {
    public short DIVISION = 0;
    public short LIST = 1;

    public XActionItem[] getActionItems();
    public short getGroupFactor();

    /*
     * Returns action of this list or its sublist,
     * by path which consists of names separated by '.'.
     */
    public XAction getAction(String path);
}

