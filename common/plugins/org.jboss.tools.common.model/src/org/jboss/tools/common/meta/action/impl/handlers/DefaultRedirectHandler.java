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
package org.jboss.tools.common.meta.action.impl.handlers;

import java.util.*;

import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.meta.action.impl.AbstractHandler;

public class DefaultRedirectHandler extends AbstractHandler implements XRedirect {

    public DefaultRedirectHandler() {}
    
    public final XModelObject getRedirectSource(XModelObject source) {
    	return getTrueSource(source);
    }
    public final XAction getRedirectAction(XModelObject source) {
    	return getTrueAction(source);
    }

    protected XModelObject getTrueSource(XModelObject source) {
        String path = action.getProperty("sourcepath"); //$NON-NLS-1$
        return (path == null) ? null : source.getChildByPath(path);
    }

    protected XAction getTrueAction(XModelObject source) {
        XModelObject ts = getTrueSource(source);
        if (ts == null) return null;
        String apath = action.getProperty("actionpath"); //$NON-NLS-1$
        if(apath == null) return null;
        StringTokenizer st = new StringTokenizer(apath, XModelObjectConstants.SEPARATOR);
        XActionItem l = ts.getModelEntity().getActionList();
        while(st.hasMoreElements()) {
            l = l.getItem(st.nextToken());
            if(l == null) return null;
        }
        return (l instanceof XAction) ? (XAction)l : null;
    }

    public boolean isEnabled(XModelObject object) {
        XAction a = getTrueAction(object);
        return (a != null && a.isEnabled(getTrueSource(object)));
    }

    public boolean isEnabled(XModelObject object, XModelObject[] objects) {
        boolean s = super.isEnabled(object, objects);
        if(!s || objects == null || objects.length < 2) return s;
        XAction a = getTrueAction(object);
        XModelObject o = getTrueSource(object);
        XModelObject[] os = new XModelObject[objects.length];
        for (int i = 0; i < os.length; i++) {
            os[i] = getTrueSource(objects[i]);
            if(os[i] == null) return false;
        }
        return (a != null && o != null && a.isEnabled(o, os));
    }

    public boolean getSignificantFlag(XModelObject object) {
        XAction a = getTrueAction(object);
        return (a != null && a.getSignificantFlag(getTrueSource(object)));
    }

    public XEntityData[] getEntityData(XModelObject object) {
        XAction a = getTrueAction(object);
        return (a == null) ? new XEntityData[0] : a.getEntityData(getTrueSource(object));
    }

    public void executeHandler(XModelObject object, Properties p) throws XModelException {
        if(!isEnabled(object)) return;
        XAction a = getTrueAction(object);
        a.executeHandler(getTrueSource(object), p);
    }

    protected void checkEnvironment(Object environment) {
        if(environment == null || !(environment instanceof Object[])) return;
        Object[] os = (Object[])environment;
        if(os.length == 0 || !(os[0] instanceof XModelObject)) return;
        os[0] = getTrueSource((XModelObject)os[0]);
    }

}

