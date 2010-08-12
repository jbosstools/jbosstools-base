/*******************************************************************************
 * Copyright (c) 2007-2009 Red Hat, Inc.
 * Distributed under license by Red Hat, Inc. All rights reserved.
 * This program is made available under the terms of the
 * Eclipse Public License v1.0 which accompanies this distribution,
 * and is available at http://www.eclipse.org/legal/epl-v10.html
 *
 * Contributor:
 *     Red Hat, Inc. - initial API and implementation
 ******************************************************************************/
package org.jboss.tools.ui.bot.ext.helper;

import java.io.File;
import java.io.FileFilter;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

import org.eclipse.swtbot.swt.finder.SWTBot;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotText;
import org.eclipse.swtbot.swt.finder.widgets.SWTBotTree;
import org.jboss.tools.ui.bot.ext.types.IDELabel;
import org.jboss.tools.ui.bot.ext.view.PackageExplorer;

/**
 * Contains some file manipulation utils 
 * @author Vladimir Pakan
 *
 */
public class FileHelper {
  /**
   * Recursively copies files and subdirectories from fromLocation to toLocation using FileFilter fileFliter
   * @param fromLocation
   * @param toLocation
   * @param fileFilter
   * @throws IOException
   */
  public static void copyFilesBinaryRecursively(File fromLocation, File toLocation, FileFilter fileFilter ) throws IOException{
    if (fromLocation.exists()){
      for (File fileToCopy : fromLocation.listFiles(fileFilter)){
        if (fileToCopy.isDirectory()){
          File newToDir = new File(toLocation, fileToCopy.getName());
          newToDir.mkdir();
          copyFilesBinaryRecursively (fileToCopy, newToDir, fileFilter);
        }
        else{
          copyFilesBinary(fileToCopy,toLocation);
        }
      }
    }
  }
  /**
   * Copies binary file originalFile to location toLocation
   * @param originalFile
   * @param toLocation
   * @throws IOException 
   */
  public static void copyFilesBinary(File originalFile, File toLocation) throws IOException{
    FileInputStream fis = null;
    FileOutputStream fos = null;
    try {
      fis = new FileInputStream(originalFile);
      fos = new FileOutputStream(new File(toLocation, originalFile.getName()));
      byte[] buffer = new byte[4096];
      int bytesRead;

      while ((bytesRead = fis.read(buffer)) != -1) {
        fos.write(buffer, 0, bytesRead); // write
      }
      
    }finally {
      if (fis != null) {
        try {
          fis.close();
        } catch (IOException e) {
          // do nothing
        }
      }
      if (fos != null) {
        try {
          fos.flush();
          fos.close();
        } catch (IOException e) {
          // do nothing
        }
      }
    }
  }

  public static String getProjectLocation (String projectName , SWTBot bot){
    PackageExplorer packageExplorer = new PackageExplorer();
    SWTBot packageExplorerBot = packageExplorer.show().bot();
    SWTBotTree packageExplorerTree = packageExplorerBot.tree();
    packageExplorerTree.expandNode(projectName).select();
    bot.menu(IDELabel.Menu.FILE).menu(IDELabel.Menu.PROPERTIES).click();
    SWTBot propertiesBot = bot.shell(IDELabel.Shell.PROPERTIES_FOR + " " + projectName).activate().bot();
    propertiesBot.tree().select(IDELabel.PropertiesWindow.RESOURCE);
    SWTBotText pathText = propertiesBot.textWithLabel(IDELabel.PropertiesWindow.ResourceProperties.LOCATION);
    String projectLocation = pathText.getText();
    propertiesBot.button(IDELabel.Button.OK).click();
    return projectLocation;
  }
  
}
