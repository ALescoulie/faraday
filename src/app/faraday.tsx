import * as React from 'react';
import Tabs from '@mui/material/Tabs';
import Tab from '@mui/material/Tab';
import Typography from '@mui/material/Typography';
import Box from '@mui/material/Box';
import List from '@mui/material/List';
import ListItem from '@mui/material/ListItem';
import ListItemIcon from '@mui/material/ListItemIcon';
import ListItemButton from '@mui/material/ListItemButton';
import ListItemText from '@mui/material/ListItemText';
import CircleIcon from '@mui/icons-material/Circle';
import ChevronLeftIcon from '@mui/icons-material/ChevronLeft'
import CircleOutlined from '@mui/icons-material/CircleOutlined';
import DeleteIcon from '@mui/icons-material/Delete';
import MenuIcon from '@mui/icons-material/Menu';
import Checkbox from '@mui/material/Checkbox';
import TextField from '@mui/material/TextField';
import IconButton from '@mui/material/IconButton';
import Drawer from '@mui/material/Drawer';
import Divider from '@mui/material/Divider';
import {styled, useTheme} from '@mui/material/styles';
import CssBaseline from '@mui/material/CssBaseline';
import Toolbar from '@mui/material/Toolbar';
import MuiAppBar, { AppBarProps as MuiAppBarProps } from '@mui/material/AppBar';
import Stack from '@mui/material/Stack';
import Paper from '@mui/material/Paper';


interface InputLineProps {
    index: number;
    content: string;
    graph: boolean;

}


// Toggels graphing a thing on and off
function setChecked(event: React.ChangeEvent<HTMLInputElement>,
                    index: number) {
    if (event.target.checked) {
        console.log("checked");
    } else {
        console.log("unchecked");
    }
};


function InputLineItem (
    index: number,
    item: string,
    handleInputChange: (index: number, item: string) => void,
    handleInputDelete: (index: number) => void
    ) 
{

    return (
        <ListItem
            key={index}
            secondaryAction={
              <IconButton edge="end" aria-label="comments" index={index} onClick={() => handleInputDelete(index)}>
                <DeleteIcon />
              </IconButton>
            }
            disablePadding
        >
            <ListItemButton role={undefined}  dense>
                <ListItemIcon>
                    <Checkbox
                        icon={<CircleIcon/>}
                        checkedIcon={<CircleOutlined />}
                        defaultChecked
                        disabled={true}
                        onChange={(event) => setChecked(event, index)}
                    />
                </ListItemIcon>
                <ListItemText
                    id={`input${index}`}
                    primary={
                        <TextField
                            id={`input ${index}`}
                            value={item}
                            multiline
                            variant="filled"
                            onChange={(e) => (handleInputChange(index, e.target.value))} 
                        />
                             }
                >
                </ListItemText>
            </ListItemButton>
        </ListItem>    
    );
}


const drawerWidth = 20


const DrawerHeader = styled('div')(({ alignment }) => ({
    display: 'flex',
    alignItems: alignment,
    justifyContent: 'flex-end'
}));

function InputDrawer(
    open: boolean,
    drawerCloseHandler: any,
    lineInputItems: string[],
    handleInputChange: (index: number, value: string) => void,
    handleInputDelete: (index: number) => void
    )
{

    return (
        <Drawer
            sx={{
                width: `${drawerWidth}%`,
                flexShrink: 0
            }}
            variant="persistent"
            anchor="left"
            open={open}
        >
            <DrawerHeader>
                <IconButton onClick={drawerCloseHandler}>
                    <ChevronLeftIcon />
                </IconButton>
            </DrawerHeader>
            <Divider />
            <List>
                {
                    lineInputItems.map((item, index) => (
                        InputLineItem(index,
                                      item,
                                      (index: number, item: string) => (handleInputChange(index, item)),
                                      (index: number) => (handleInputDelete(index))
                                     )
                        )
                    )
                }
            </List>
        </Drawer>
    )
}


const tabsSize = 48


function FaradayUI () {

    const [open, setOpen] = React.useState(false);
    const [mentatLines, setMentatLines] = React.useState(['']);
    

    const handleDrawerOpen = () => {
        setOpen(true);
    };

    const handleDrawerClose = () => {
        setOpen(false);
    };

    const Main = styled('main', { shouldForwardProp: (prop) => prop !== 'open' })<{
      open?: boolean;
    }>(({ theme, open }) => ({
      flexGrow: 1,
      padding: theme.spacing(3),
      transition: theme.transitions.create('margin', {
        easing: theme.transitions.easing.sharp,
        duration: theme.transitions.duration.leavingScreen,
      }),
      marginLeft: `-${drawerWidth}%`,
      ...(open && {
        transition: theme.transitions.create('margin', {
          easing: theme.transitions.easing.easeOut,
          duration: theme.transitions.duration.enteringScreen,
        }),
        marginLeft: 0,
      }),
    }));


    const AppBar = styled(MuiAppBar, {
      shouldForwardProp: (prop) => prop !== 'open',
    })<AppBarProps>(({ theme, open }) => ({
      transition: theme.transitions.create(['margin', 'width'], {
        easing: theme.transitions.easing.sharp,
        duration: theme.transitions.duration.leavingScreen,
      }),
      ...(open && {
        width: `calc(100% - ${drawerWidth}%)`,
        marginLeft: `${drawerWidth}%`,
        transition: theme.transitions.create(['margin', 'width'], {
          easing: theme.transitions.easing.easeOut,
          duration: theme.transitions.duration.enteringScreen,
        }),
      }),
    }));

    const handleInputChange = (index: number, item: string) => {
        const newItems = [...mentatLines];
        newItems[index] = item;
        
        if (index === newItems.length - 1) {
            setMentatLines([...newItems, '']);
        } else {
            setMentatLines(newItems);
        }
    };

    const handleInputDelete = (index: number) => {
        const newItems = [...mentatLines];
        console.log(`deleting ${index} from current items: ${newItems}`);
        if (newItems.length === 1) {
            setMentatLines(['']);
        } else {
            setMentatLines(newItems.slice(0, index).concat(newItems.slice(index + 1)));
        }
    };


    return (
        <Box sx={{ display: 'flex', marginTop: `${tabsSize}px`}}>
            <CssBaseline />
            <AppBar position="fixed" open={open}>
            <Toolbar>
                <IconButton
                    color="inherit"
                    aria-label="open drawer"
                    onClick={handleDrawerOpen}
                    edge="start"
                    sx={{ mr: 2, ...(open && { display: 'none' }) }}
                >
                    <MenuIcon />
                </IconButton>
                <Typography variant="h6" noWrap component="div">
                    Faraday
                </Typography>
            </Toolbar>
            </AppBar>
            {InputDrawer(open, handleDrawerClose, mentatLines, handleInputChange, handleInputDelete)}
            <Main open={open}>
                <DrawerHeader />
                <Typography paragraph>
                    {mentatLines.toString()}
                </Typography>
            </Main>
        </Box>
    )
}


const Item = styled(Paper)(({ theme }) => ({
  backgroundColor: theme.palette.mode === 'dark' ? '#1A2027' : '#fff',
  ...theme.typography.body2,
  padding: theme.spacing(0.5),
  textAlign: 'center',
  color: theme.palette.text.secondary,
}));

function Faraday() {


    return (
        <Box sx={{width: '100%'}}>
            {FaradayUI()}
        </Box>
    );
}

export default Faraday;

