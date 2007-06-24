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
package org.jboss.ide.eclipse.core.test.util;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;

public class TestFileUtil
{
    public static void copyDirectory (File srcDir, File destDir, boolean recurse) throws IOException
    {
        if (srcDir.isDirectory())
        {
            File[] files = srcDir.listFiles();
            for (int i = 0; i < files.length; i++)
            {
                File copyDest = new File(destDir, files[i].getName());
                if (files[i].isDirectory())
                {
                    copyDest.mkdirs();
                    
                    if (recurse)
                        copyDirectory(files[i], copyDest, true);
                }
                else {
                    copy (files[i], copyDest);
                }
            }
        }
    }
    
    public static void copy(File src, File dst) throws IOException {
        InputStream in = new FileInputStream(src);
        OutputStream out = new FileOutputStream(dst);
    
        byte[] buf = new byte[1024];
        int len;
        while ((len = in.read(buf)) > 0) {
            out.write(buf, 0, len);
        }
        in.close();
        out.close();
    }
    
    public static boolean projectFileExists (IProject project, String path)
    {
       return projectFileExists (project, new Path(path));
    }
    
    public static boolean projectFileExists (IProject project, IPath path)
    {
       IFile file = project.getFile(path);
       return file.exists();
    }
}
