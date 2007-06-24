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
package org.jboss.tools.common.model.search.impl;

import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.search.*;
import org.jboss.tools.common.model.impl.*;
import org.jboss.tools.common.model.util.*;

public class SearchCommandImpl extends SearchConstraintImpl implements SearchCommand {
	private static final long serialVersionUID = 1L;
    protected SearchConstraint matching;
    protected SearchConstraint insight;

    public SearchCommandImpl() {}

    public void prepare() {
        super.prepare();
        matching = (SearchConstraint)getChildren("SearchMatch")[0];
        insight = (SearchConstraint)getChildren("SearchRecursion")[0];
    }

    public XModelObject[] execute() {
        prepare();
        ArrayList<XModelObject> list = new ArrayList<XModelObject>();
        String[] roots = XModelObjectUtil.asStringArray(getAttributeValue("root"));
        ArrayList<XModelObject> os = new ArrayList<XModelObject>();
        if(roots.length == 0) {
            os.add(getModel().getRoot());
        } else {
            for (int i = 0; i < roots.length; i++) {
                XModelObject root = getModel().getByPath(roots[i]);
                if(root != null) os.add(root);
            }
        }
        for (int i = 0; i < os.size(); i++) {
            XModelObject root = os.get(i);
            execute(root, list);
        }
        return list.toArray(new XModelObject[list.size()]);
    }

    public void validate() {
        String rootpath = getAttributeValue("root");
        if(rootpath.length() == 0) throw new RuntimeException("Attribute 'root' of search command must be set.");
        XModelObject root = getModel().getByPath(rootpath);
        if(root == null) throw new RuntimeException("Root of search command is not found in model.");
    }

    protected void execute(XModelObject o, ArrayList<XModelObject> list) {
        if(matching.accepts(o)) list.add(o);
        if(insight.accepts(o)) {
            XModelObject[] os = ((XModelObjectImpl)o).getChildrenForSave();
            for (int i = 0; i < os.length; i++) execute(os[i], list);
        }
    }

}
