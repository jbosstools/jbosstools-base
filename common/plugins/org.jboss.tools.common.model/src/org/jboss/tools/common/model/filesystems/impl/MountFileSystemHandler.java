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
package org.jboss.tools.common.model.filesystems.impl;

import java.util.*;
import java.io.*;
import org.jboss.tools.common.meta.action.*;
import org.jboss.tools.common.meta.action.impl.handlers.DefaultCreateHandler;
import org.jboss.tools.common.model.*;
import org.jboss.tools.common.model.util.*;
import org.jboss.tools.common.model.impl.XModelClassLoader;

public class MountFileSystemHandler extends DefaultCreateHandler {

    public MountFileSystemHandler() {}

    public void executeHandler(XModelObject object, Properties p) throws Exception {
        if(!isEnabled(object) || data == null || data.length == 0) return;
        String entity = data[0].getModelEntity().getName();
        p = extractProperties(data[0]);
        if(!checkOverlap(object, entity, p)) return;
        setRelativeToProject(object, p);
        mount(object, p, entity);
    }

    public XModelObject mount(XModelObject fs, Properties p, String entity) {
        validateName(fs, p);
        XModelObject c = XModelObjectLoaderUtil.createValidObject(fs.getModel(), entity, p);
        addCreatedObject(fs, c, false, p);
        fs.getModel().getUndoManager().addUndoable(new MountFileSystemUndo(c));
        updateClassPath(c);
		MoveFileSystemHandler.sortFileSystems(fs.getModel());
        return c;
    }

    private boolean checkOverlap(XModelObject object, String entity, Properties p) {
        String location = p.getProperty("location");
        if(location == null) return true;
        boolean b = location.indexOf('%') >= 0;
        location = canonize(location, object.getModel());
        if(location != null && !b) p.setProperty("location", location);
        if(!"FileSystemFolder".equals(entity)) return true;
        location += "/";
        XModelObject[] cs = object.getChildren(entity);
        for (int i = 0; i < cs.length; i++) {
            String loc = canonize(cs[i].get("LOCATION"), cs[i].getModel()) + "/";
            if(!loc.startsWith(location) && !location.startsWith(loc)) continue;
            String mes = "File system " + p.get("name") + " will share files with file system " + cs[i].getAttributeValue("name");
            ServiceDialog d = object.getModel().getService();
            int q = d.showDialog("Warning", mes, new String[]{"OK", "Cancel"}, null, ServiceDialog.WARNING);
            return (q == 0);
        }
        return true;
    }

    private String canonize(String location, XModel model) {
        try {
            location = XModelObjectUtil.expand(location, model, null);
            return (new File(location).getCanonicalPath()).replace('\\', '/');
        } catch (Exception e) {
            return location;
        }
    }

    private void validateName(XModelObject object, Properties p) {
        String name = p.getProperty("name");
        if(name != null && name.length() > 0) return;
        String location = p.getProperty("location");
        name = location.substring(location.lastIndexOf('/') + 1);
        if(name.length() == 0) name = "filesystem";
        name = XModelObjectUtil.createNewChildName(name, object);
        p.setProperty("name", name);
    }

    private void setRelativeToProject(XModelObject object, Properties p) {
        boolean isRelative = "true".equals(p.getProperty("set location relative to project"));
        if(!isRelative) return;
        String location = canonize(p.getProperty("location"), object.getModel());
        String project = canonize("%redhat.workspace%", object.getModel());
        if(location.equals(project)) {
            p.setProperty("location", "%redhat.workspace%");
            return;
        }
        boolean common = false;
        while(location.length() > 0 && project.length() > 0) {
            int i1 = location.indexOf('/');
            if(i1 < 0) i1 = location.length();
            int i2 = project.indexOf('/');
            if(i2 < 0) i2 = project.length();
            if(i1 != i2) break;
            String p1 = location.substring(0, i1);
            String p2 = project.substring(0, i2);
            if(!p1.equals(p2)) break;
            location = location.substring(i1);
            project = project.substring(i2);
            if(location.startsWith("/")) location = location.substring(1);
            if(project.startsWith("/")) project = project.substring(1);
            common = true;
        }
        if(!common) return;
        String s = "%redhat.workspace%";
        if(project.length() > 0) {
            int q = new StringTokenizer(project, "/").countTokens();
            for (int i = 0; i < q; i++) s += "/..";
        }
        if(location.length() > 0) s += "/" + location;
        p.setProperty("location", s);
    }

	static SpecialWizard w = SpecialWizardFactory.createSpecialWizard("org.jboss.tools.common.model.project.ClassPathUpdateWizard");

    public static void updateClassPath(XModelObject fs) {
		XModelClassLoader cl = (XModelClassLoader)fs.getModel().getModelClassLoader();
		cl.invalidate();
		if(fs.getModelEntity().getName().indexOf("ar") >= 0 && w != null) {
			Properties p = new Properties();
			p.put("model", fs.getModel());
			w.setObject(p);
			w.execute();
		}
    }

}

