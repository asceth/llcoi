/******************************************************************************
 * Ogre::ResourceGroupManager bindings
 ******************************************************************************/

#include "ogre_prerequisites.h"
#include "ogre_resource_group_manager.h"

#include <OgreRoot.h>
#include <OgreConfigFile.h>
#include <OgreResourceManager.h>
#include <OgreResourceGroupManager.h>
#include <OgreResource.h>


void setup_resources(const char* resources_cfg)
{
    // set up resources
    // Load resource paths from config file
    Ogre::ConfigFile cf;
    cf.load(resources_cfg);

    // Go through all sections & settings in the file
    Ogre::ConfigFile::SectionIterator seci = cf.getSectionIterator();

    Ogre::String secName, typeName, archName;
    while (seci.hasMoreElements())
    {
        secName = seci.peekNextKey();
        Ogre::ConfigFile::SettingsMultiMap *settings = seci.getNext();
        Ogre::ConfigFile::SettingsMultiMap::iterator i;
        for (i = settings->begin(); i != settings->end(); ++i)
        {
            typeName = i->first;
            archName = i->second;
            Ogre::ResourceGroupManager::getSingleton().addResourceLocation(
                archName, typeName, secName);
        }
    }
}

// Ogre::ResourceGroupManager::addResourceLocation(std::string const&, std::string const&, std::string const&, bool)
void add_resource_location(const char* location, const char* type, const char* group)
{
    Ogre::ResourceGroupManager::getSingleton().addResourceLocation(location, type, group);
}

// Ogre::ResourceGroupManager::initialiseAllResourceGroups()
void initialise_all_resource_groups()
{
    Ogre::ResourceGroupManager::getSingleton().initialiseAllResourceGroups();
}

int resource_exists(const char* group, const char* name)
{
  if (Ogre::ResourceGroupManager::getSingleton().resourceExists(group, name))
    {
      return 1;
    }
  return 0;
}


/*
Ogre::ResourceGroupManager::DEFAULT_RESOURCE_GROUP_NAME
Ogre::ResourceGroupManager::INTERNAL_RESOURCE_GROUP_NAME
Ogre::ResourceGroupManager::AUTODETECT_RESOURCE_GROUP_NAME
Ogre::ResourceGroupManager::RESOURCE_SYSTEM_NUM_REFERENCE_COUNTS
Ogre::ResourceGroupManager::ResourceDeclaration
Ogre::ResourceGroupManager::ResourceLocation
Ogre::ResourceGroupManager::ResourceGroupManager()
Ogre::ResourceGroupManager::~ResourceGroupManager()
Ogre::ResourceGroupManager::createResourceGroup(std::string const&, bool)
Ogre::ResourceGroupManager::initialiseResourceGroup(std::string const&)
Ogre::ResourceGroupManager::prepareResourceGroup(std::string const&, bool, bool)
Ogre::ResourceGroupManager::loadResourceGroup(std::string const&, bool, bool)
Ogre::ResourceGroupManager::unloadResourceGroup(std::string const&, bool)
Ogre::ResourceGroupManager::unloadUnreferencedResourcesInGroup(std::string const&, bool)
Ogre::ResourceGroupManager::clearResourceGroup(std::string const&)
Ogre::ResourceGroupManager::destroyResourceGroup(std::string const&)
Ogre::ResourceGroupManager::isResourceGroupInitialised(std::string const&)
Ogre::ResourceGroupManager::isResourceGroupLoaded(std::string const&)
Ogre::ResourceGroupManager::resourceGroupExists(std::string const&)
Ogre::ResourceGroupManager::removeResourceLocation(std::string const&, std::string const&)
Ogre::ResourceGroupManager::resourceLocationExists(std::string const&, std::string const&)
Ogre::ResourceGroupManager::declareResource(std::string const&, std::string const&, std::string const&, std::map<std::string, std::string, std::less<std::string>, Ogre::STLAllocator<std::pair<std::string const, std::string>, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > > const&)
Ogre::ResourceGroupManager::declareResource(std::string const&, std::string const&, std::string const&, Ogre::ManualResourceLoader*, std::map<std::string, std::string, std::less<std::string>, Ogre::STLAllocator<std::pair<std::string const, std::string>, Ogre::CategorisedAllocPolicy<(Ogre::MemoryCategory)0> > > const&)
Ogre::ResourceGroupManager::undeclareResource(std::string const&, std::string const&)
Ogre::ResourceGroupManager::openResource(std::string const&, std::string const&, bool, Ogre::Resource*)
Ogre::ResourceGroupManager::openResources(std::string const&, std::string const&)
Ogre::ResourceGroupManager::listResourceNames(std::string const&, bool)
Ogre::ResourceGroupManager::listResourceFileInfo(std::string const&, bool)
Ogre::ResourceGroupManager::findResourceNames(std::string const&, std::string const&, bool)
Ogre::ResourceGroupManager::resourceExists(std::string const&, std::string const&)
Ogre::ResourceGroupManager::resourceExists(Ogre::ResourceGroupManager::ResourceGroup*, std::string const&)
Ogre::ResourceGroupManager::resourceExistsInAnyGroup(std::string const&)
Ogre::ResourceGroupManager::findGroupContainingResource(std::string const&)
Ogre::ResourceGroupManager::findResourceFileInfo(std::string const&, std::string const&, bool)
Ogre::ResourceGroupManager::resourceModifiedTime(std::string const&, std::string const&)
Ogre::ResourceGroupManager::listResourceLocations(std::string const&)
Ogre::ResourceGroupManager::findResourceLocation(std::string const&, std::string const&)
Ogre::ResourceGroupManager::resourceModifiedTime(Ogre::ResourceGroupManager::ResourceGroup*, std::string const&)
Ogre::ResourceGroupManager::createResource(std::string const&, std::string const&, bool, std::string const&)
Ogre::ResourceGroupManager::deleteResource(std::string const&, std::string const&, std::string const&)
Ogre::ResourceGroupManager::deleteMatchingResources(std::string const&, std::string const&, std::string const&)
Ogre::ResourceGroupManager::addResourceGroupListener(Ogre::ResourceGroupListener*)
Ogre::ResourceGroupManager::removeResourceGroupListener(Ogre::ResourceGroupListener*)
Ogre::ResourceGroupManager::setWorldResourceGroupName(std::string const&)
Ogre::ResourceGroupManager::getWorldResourceGroupName() const
Ogre::ResourceGroupManager::linkWorldGeometryToResourceGroup(std::string const&, std::string const&, Ogre::SceneManager*)
Ogre::ResourceGroupManager::unlinkWorldGeometryFromResourceGroup(std::string const&)
Ogre::ResourceGroupManager::isResourceGroupInGlobalPool(std::string const&)
Ogre::ResourceGroupManager::shutdownAll()
Ogre::ResourceGroupManager::_registerResourceManager(std::string const&, Ogre::ResourceManager*)
Ogre::ResourceGroupManager::_unregisterResourceManager(std::string const&)
Ogre::ResourceGroupManager::getResourceManagerIterator()
Ogre::ResourceGroupManager::_registerScriptLoader(Ogre::ScriptLoader*)
Ogre::ResourceGroupManager::_unregisterScriptLoader(Ogre::ScriptLoader*)
Ogre::ResourceGroupManager::_findScriptLoader(std::string const&)
Ogre::ResourceGroupManager::_getResourceManager(std::string const&)
Ogre::ResourceGroupManager::_notifyResourceCreated(Ogre::SharedPtr<Ogre::Resource>&)
Ogre::ResourceGroupManager::_notifyResourceRemoved(Ogre::SharedPtr<Ogre::Resource>&)
Ogre::ResourceGroupManager::_notifyResourceGroupChanged(std::string const&, Ogre::Resource*)
Ogre::ResourceGroupManager::_notifyAllResourcesRemoved(Ogre::ResourceManager*)
Ogre::ResourceGroupManager::_notifyWorldGeometryStageStarted(std::string const&)
Ogre::ResourceGroupManager::_notifyWorldGeometryStageEnded()
Ogre::ResourceGroupManager::getResourceGroups()
Ogre::ResourceGroupManager::getResourceDeclarationList(std::string const&)
Ogre::ResourceGroupManager::getResourceLocationList(std::string const&)
Ogre::ResourceGroupManager::setLoadingListener(Ogre::ResourceLoadingListener*)
Ogre::ResourceGroupManager::getLoadingListener()
Ogre::ResourceGroupManager::getSingleton()
Ogre::ResourceGroupManager::getSingletonPtr()
Ogre::ResourceGroupManager::ResourceDeclaration::~ResourceDeclaration()
Ogre::ResourceGroupManager::ResourceDeclaration::operator=(Ogre::ResourceGroupManager::ResourceDeclaration const&)
Ogre::ResourceGroupManager::ResourceDeclaration::ResourceDeclaration(Ogre::ResourceGroupManager::ResourceDeclaration const&)
Ogre::ResourceGroupManager::ResourceDeclaration::ResourceDeclaration()
Ogre::ResourceGroupManager::ResourceLocation::~ResourceLocation()
Ogre::ResourceGroupManager::ResourceLocation::operator=(Ogre::ResourceGroupManager::ResourceLocation const&)
Ogre::ResourceGroupManager::ResourceLocation::ResourceLocation(Ogre::ResourceGroupManager::ResourceLocation const&)
Ogre::ResourceGroupManager::ResourceLocation::ResourceLocation()
Ogre::ResourceGroupManager::ResourceGroup::~ResourceGroup()
Ogre::ResourceGroupManager::ResourceGroup::ResourceGroup()
Ogre::ResourceGroupManager::ResourceGroup::addToIndex(std::string const&, Ogre::Archive*)
Ogre::ResourceGroupManager::ResourceGroup::removeFromIndex(std::string const&, Ogre::Archive*)
Ogre::ResourceGroupManager::ResourceGroup::removeFromIndex(Ogre::Archive*)
*/
