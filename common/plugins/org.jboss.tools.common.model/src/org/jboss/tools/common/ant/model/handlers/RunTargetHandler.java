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
package org.jboss.tools.common.ant.model.handlers;

import java.util.*;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.impl.*;
import org.jboss.tools.common.model.engines.impl.*;
import org.jboss.tools.common.meta.action.impl.*;
import org.jboss.tools.common.model.filesystems.XFileObject;
import org.jboss.tools.common.model.filesystems.impl.FileAnyImpl;
import org.jboss.tools.common.model.util.*;

public class RunTargetHandler extends AbstractHandler {

    public RunTargetHandler() {}

    public boolean isEnabled(XModelObject object) {
        return (object != null);
    }

    public void executeHandler(XModelObject object, Properties p) throws Exception {
        if(!isEnabled(object)) return;
        RunAntProcess process = new RunAntProcess(object);
        process.start();
        new Thread(new U(object.getModel(), process)).start();
        XProcessStorage.getDefaultStorage().addInstance(object.getPath(), process);
    }

    class U implements Runnable {
        XModel model;
        XProcess process;
        U(XModel model, XProcess process) {
            this.model = model;
            this.process = process;
        }
        public void run() {
            process.waitFor();
            model.update();
        }
    }

}

class RunAntProcess extends XProcess {
    private XModelObject js = null;

    public RunAntProcess(XModelObject js) {
        this.js = js;
    }

    protected String getRoot() {
        return ".";
    }

    private String qualifiedName() {
        String r = "";
        XModelObject p = (js.getFileType() == XFileObject.FILE) ? js : js.getParent();
        r = FileAnyImpl.toFileName(p);
        p = p.getParent();
        while(p != null && p.getFileType() != XFileObject.SYSTEM) {
            r = p.getAttributeValue("name") + "/" + r;
            p = p.getParent();
        }
        if(p == null) return null;
        r = p.get("LOCATION") + "/" + r;
        r = XModelObjectUtil.expand(r, p.getModel(), null);
        return r;
    }

    protected void buildCommandLine(ArrayList<String> l) {
        appendJava(l, js.getModel().getProperties());
        l.add("-classpath");
        l.add(getClassPath());
        l.add("org.apache.tools.ant.Main");
        l.add("-buildfile");
        l.add(qualifiedName());
        if(js.getFileType() != XFileObject.FILE);
          l.add(js.getAttributeValue("name"));
    }

    private String getClassPath() {
        XModelClassLoader cl = (XModelClassLoader)js.getModel().getModelClassLoader();
        return cl.getClassPath();
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

