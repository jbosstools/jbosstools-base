/*
 * JBoss, Home of Professional Open Source
 * Copyright 2005, JBoss Inc., and individual contributors as indicated
 * by the @authors tag. See the copyright.txt in the distribution for a
 * full listing of individual contributors.
 *
 * This is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation; either version 2.1 of
 * the License, or (at your option) any later version.
 *
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this software; if not, write to the Free
 * Software Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
 * 02110-1301 USA, or see the FSF site: http://www.fsf.org.
 */
package org.jboss.ide.eclipse.core.test;

import java.io.File;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;

public class CoreTestPlugin extends Plugin {

	public CoreTestPlugin() {
		super();
		// TODO Auto-generated constructor stub
	}

	public void start(BundleContext context) throws Exception {
		// TODO Auto-generated method stub
		super.start(context);
	}
	
	public void stop(BundleContext context) throws Exception {
		// TODO Auto-generated method stub
		super.stop(context);
	}
	
    /**
     * Get the base directory of this plugin
     */
    public String getBaseDir()
    {
        try
        {
            URL installURL = Platform.asLocalURL(this.getBundle().getEntry("/"));//$NON-NLS-1$
            return installURL.getFile().toString();
        }
        catch (IOException ioe)
        {
            ioe.printStackTrace();
        }
        return null;
    }
    
    public static URL getRawLocationURL(IPath simplePath) throws MalformedURLException {
        File file = getRawLocationFile(simplePath);
        return file.toURL();
    }

    public static File getRawLocationFile(IPath simplePath) {
        IResource resource = ResourcesPlugin.getWorkspace().getRoot().findMember(simplePath);
        File file = null;
        if(resource!=null) {
            file = ResourcesPlugin.getWorkspace().getRoot().findMember(simplePath).getRawLocation().toFile();   
        } else {
            file = simplePath.toFile();
        }
        return file;
    }
    public IPath getProjectLocation (IProject project)
    {
        if (project.getRawLocation() == null)
        {
            return project.getLocation();
        }
        else return project.getRawLocation();
    }
}
