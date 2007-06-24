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
package org.jboss.tools.common.java.model.handler;

import java.io.*;
import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.model.engines.impl.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.model.util.FindObjectHelper;
import org.jboss.tools.common.model.impl.XModelClassLoader;

public class RunJavaHandler extends AbstractHandler {

    public RunJavaHandler() {}

    public boolean isEnabled(XModelObject object) {
        if(object == null) return false;
        object = object.getChildByPath(object.getAttributeValue("name"));
        if(object == null) return false;
        XModelObject m = object.getChildByPath("main(String[])");
        if(m == null) m = object.getChildByPath("main(java.lang.String[])");
        return (m != null);
    }

    public void executeHandler(XModelObject object, Properties p) throws Exception {
        if(!isEnabled(object)) return;
        RunJavaProcess process = new RunJavaProcess(object);
        process.start();
        XProcessStorage.getDefaultStorage().addInstance(object.getPath(), process);
    }

}

class RunJavaProcess extends XProcess {
    private XModelObject js = null;
    private String qn = null;

    public RunJavaProcess(XModelObject js) {
        this.js = js;
        qn = qualifiedName();
    }

    protected String getRoot() {
        return ".";
    }

    private String qualifiedName() {
        String p = XModelObjectLoaderUtil.getResourcePath(js);
        return p.substring(1, p.length() - 5).replace('/', '.');
    }

    protected void buildCommandLine(ArrayList<String> l) {
        appendJava(l, js.getModel().getProperties());
        l.add("-classpath");
        l.add(getClassPath());
        l.add(qn);
    }

    private String getClassPath() {
        XModelClassLoader l = (XModelClassLoader)js.getModel().getModelClassLoader();
        String s = l.getClassPath();
        XModelObject g = js.getModel().getByPath("Engines/generator");
        String bo = XModelObjectUtil.getExpandedValue(g, "directory", null) + "/classes";
        s += File.pathSeparator + bo;
        return s;
    }

    protected void write(String s) {
        StringTokenizer st = new StringTokenizer(s, "\n");
        while(st.hasMoreTokens()) {
            String n = st.nextToken();
            n = FindObjectHelper.enhanceStackTraceEntry(js.getModel(), n);
            js.getModel().getOut().println(n);
        }
    }

}

